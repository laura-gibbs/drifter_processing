import os
from time import time
import numpy as np
import pandas as pd
import netCDF4 as nc
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
from cartopy.feature import GSHHSFeature
from cartopy.mpl.ticker import LongitudeFormatter, LatitudeFormatter
import xarray as xr
from scipy import interpolate
from datetime import date
import math


def read_dat_file(path, filename):
    filepath = os.path.join(os.path.normpath(path), filename)
    fid = open(filepath, mode='r')
    df = pd.read_csv(fid, sep="\s+", header=None)
    headers = ['id', 'mm', 'dd', 'yy', 'lat', 'lon', 'temp', 've', 'vn', 'spd', 'var_lat', 'var_lon', 'var_temp']
    df.columns = headers
    df = df.loc[df["spd"]<999.999]
    return df


def return_important_drifter_variables(df):
    df = df.drop(df.columns[[6,10,11,12]], axis=1)
    return df


def plot_drifter_trajectories(df, direction=None, global_extent=False):
    ids = df['id'].unique()

    for id in ids:
        # df_id = df_id[1:-1]
        df_id = df.loc[df['id']==id]
        fig = plt.figure(figsize=(20,12))
        crs = ccrs.PlateCarree()
        ax = fig.add_subplot(1, 1, 1, projection=crs)
        ax.add_feature(GSHHSFeature(scale='intermediate', facecolor='lightgrey', linewidth=0.2))
        ax.set_xticks(np.linspace(-180, 180, 19), crs=crs)
        ax.set_yticks(np.linspace(-90, 90, 19), crs=crs)
        lon_formatter = LongitudeFormatter(zero_direction_label=True)
        lat_formatter = LatitudeFormatter()
        ax.xaxis.set_major_formatter(lon_formatter)
        ax.yaxis.set_major_formatter(lat_formatter)
        if global_extent:
            ax.set_global()
        # ax.gridlines()
        if direction=='v':
            plt.scatter(x=df_id.lon, y=df_id.lat, s=5, c=df_id.vn, transform=crs, cmap='jet')
        elif direction=='u':
            plt.scatter(x=df_id.lon, y=df_id.lat, s=5, c=df_id.ve, transform=crs, cmap='jet')
        else:
            plt.scatter(x=df_id.lon, y=df_id.lat, s=5, c=df_id.spd, transform=crs, cmap='jet')
            print('Displayed speed: input v for northward velocity or u for eastward')
        plt.show()


def plot_time_step(time_step_list, direction=None):
    fig = plt.figure(figsize=(20,12))
    crs = ccrs.PlateCarree()
    ax = fig.add_subplot(1, 1, 1, projection=crs)
    ax.add_feature(GSHHSFeature(scale='intermediate', facecolor='lightgrey', linewidth=0.2))
    ax.set_xticks(np.linspace(-180, 180, 19), crs=crs)
    ax.set_yticks(np.linspace(-90, 90, 19), crs=crs)
    lon_formatter = LongitudeFormatter(zero_direction_label=True)
    lat_formatter = LatitudeFormatter()
    ax.xaxis.set_major_formatter(lon_formatter)
    ax.yaxis.set_major_formatter(lat_formatter)
    ax.set_global()
    if direction=='v':
        plt.scatter(x=time_step_list.lon, y=time_step_list.lat, s=5, c=time_step_list.vn, transform=crs, cmap='jet')
    elif direction=='u':
        plt.scatter(x=time_step_list.lon, y=time_step_list.lat, s=5, c=time_step_list.ve, transform=crs, cmap='jet')
    else:
        plt.scatter(x=time_step_list.lon, y=time_step_list.lat, s=5, c=time_step_list.spd, transform=crs, cmap='jet')
        print('Displayed speed: input v for northward velocity or u for eastward')
    plt.show()


def get_time_step(df, years, months, days):
    df_list = []
    for year in years:
        for month in months:
            for day in days:
                # df = df.loc[df["spd"]<999.999]
                df_t = df.loc[(df['yy']==year) & (df['mm']==month) & (df['dd']==day)]
                df_list.append(df_t)
    return df_list


def get_time_step_over_month(df, years, months):
    df_list = []
    for year in years:
        for month in months:
            # df = df.loc[df["spd"]<999.999]
            df_t = df.loc[(df['yy']==year) & (df['mm']==month)]
            df_list.append(df_t)
    return df_list


def get_drifter_vel(df, direction=None):
    # df = df.loc[df["spd"]<999.999]
    if direction == 'v':
        return df['vn']
    elif direction == 'u':
        return df['ve']
    else:
        return df['spd']


def add_hours_column(df):
    days, mnths, yrs = df['dd'], df['mm'], df['yy']
    conv_times = []
    for yr, mnth, day in zip(yrs, mnths, days):
        t = convert_time_to_hrs(year=yr, month=mnth, day=day)
        conv_times.append(t)
    df['hrs'] = conv_times
    return df


def convert_time_to_hrs(year, month, day):
    d_split = math.modf(day)
    d0 = date(1900, 1, 1)
    d1 = date(year, month, int(d_split[1]))
    diff = (d1 - d0)
    hours = (diff.days + d_split[0]) * 24
    return hours


def subtract_windslip(df, wind_dataset):
    times = wind_dataset.variables['time'][:]
    u10, v10 = wind_dataset.variables['u10'], wind_dataset.variables['v10']
    lats, lons = wind_dataset.variables['latitude'][:], wind_dataset.variables['longitude'][:]
    for i, time in enumerate(times):
        f_u10 = interpolate.interp2d(lons, lats, u10[i], kind='cubic')
        f_v10 = interpolate.interp2d(lons, lats, v10[i], kind='cubic')
        df_t = df.loc[df["hrs"]==time]
        indices = df.index[df["hrs"]==time]
        # any_true = np.any(df["hrs"]==time)
        # if any_true:
            # print(i, f"there was a match!!!")
        u10_new = df_t.apply(lambda x: f_u10(x['lon'], x['lat'])[0], axis=1)
        v10_new = df_t.apply(lambda x: f_v10(x['lon'], x['lat'])[0], axis=1)
        wind_slip_u = u10_new * 0.07
        wind_slip_v = v10_new * 0.07
        df.loc[indices,'corr_ve'] = df_t['ve'] - wind_slip_u
        df.loc[indices,'corr_vn'] = df_t['vn'] - wind_slip_v
        if i == 2:
            break
    return df


def main():
    fpath = 'drifters/'
    fnames = ['buoydata_1_5000.dat', 'buoydata_5001_10000.dat', 'buoydata_10001_15000.dat','buoydata_15001_dec21.dat']    
    
    df_d = read_dat_file(fpath, fnames[0])
    df_d = return_important_drifter_variables(df_d)
    df_d = add_hours_column(df_d)

    df_d['corr_ve'] = 0
    df_d['corr_vn'] = 0
    yrs = sorted(df_d['yy'].unique())
    print(yrs)
    for yr in yrs:
        print(yr)
        f_path = f'D:/era5/download_{yr}.nc'
        if os.path.exists(f_path):
            era5_name = f'D:/era5/download_{yr}.nc'
            wind_dataset = nc.Dataset(era5_name)
            df_d = subtract_windslip(df_d, wind_dataset)
        else:
            print(f'{yr} era5 file does not exist')

    df_d_1990 = df_d[df_d.yy == 1990]
    df_d_1991 = df_d[df_d.yy == 1991]
    print(df_d_1990.head(200))
    print(df_d_1991.head(200))
    # exit()
    
    plot_drifter_trajectories(df_d, 'v', global_extent=True)
    # v_drifters = get_drifter_vel(df_d, 'v')
    # u_drifters = get_drifter_vel(df_d, 'u')

    years = [1990, 1992]
    months = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
    days = np.arange(4, 5, 0.25)
    
    df_month_list = get_time_step_over_month(df_d, years, months)
    df_time_step = get_time_step(df_d, years, months, days)
    for item in df_time_step:
        plot_time_step(item, direction='v')

    # # years = np.arange(1988,2002)
    # # months = np.arange(1,13)
    # # days = np.arange(1,31,0.25)

if __name__ == '__main__':
    main()
