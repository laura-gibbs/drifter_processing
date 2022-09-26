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
from scipy import signal


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
        elif direction=='corr_filt_v':
            plt.scatter(x=df_id.filt_lon, y=df_id.filt_lat, s=5, c=df_id.corr_vn, transform=crs, cmap='jet')
        elif direction=='corr_filt_u':
            plt.scatter(x=df_id.filt_lon, y=df_id.filt_lat, s=5, c=df_id.corr_ve, transform=crs, cmap='jet')
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
    # could replace nested for loop with hrs
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


def read_dat_file(path, filename):
    filepath = os.path.join(os.path.normpath(path), filename)
    fid = open(filepath, mode='r')
    df = pd.read_csv(fid, sep="\s+", header=None)
    return df


def read_drifters_to_df(path, filename, headers):
    df = read_dat_file(path, filename)
    df.columns = headers
    df.astype(float)
    df = df.loc[df["spd"]<999.999]
    return df


def correct_velocities_wind_slip(df):
    df['corr_ve'] = 0
    df['corr_vn'] = 0
    yrs = sorted(df['yy'].unique()) 
    # yrs = [1979, 1980, 1981] #----------------- remove ------------------
    print(yrs)
    for yr in yrs:
        print(yr)
        f_path = f'D:/era5/download_{yr}.nc'
        if os.path.exists(f_path):
            era5_name = f'D:/era5/download_{yr}.nc'
            wind_dataset = nc.Dataset(era5_name)
            df = subtract_windslip(df, wind_dataset)
        else:
            print(f'{yr} era5 file does not exist')
    return df


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
    return df


def filter_inertial_motion(df):
    # print('-- filtering inertial motion --')
    ids = df['id'].unique()

    for id in ids:
        df_id = df.loc[df['id']==id]
        indices = df.index[df["id"]==id]

        corr_vn = np.array(df_id['corr_vn'])
        corr_ve = np.array(df_id['corr_ve'])
        lons = np.array(df_id['lon'])
        lats = np.array(df_id['lat'])
        lats[lats == 0] = 0.01
        n = len(lats)
        omega = 7.2921e-5
        f = 2*omega*np.sin(lats)
        T = (2*np.pi/(f))/3600
        w = (np.floor(np.abs(T/6))).astype(int)
        # print(w)

        filt_lats = np.zeros(n)
        filt_lons = np.zeros(n)
        filt_ve = np.zeros(n)
        filt_vn = np.zeros(n)
        for i in range(n):
            left_w = min(w[i], i)
            right_w = min(w[i], n-i-1)
            filt_lats[i] = np.mean(lats[i-left_w:i+right_w])
            if not filt_lats[i]:
                print(f'lat[{i}] is empty') 
            filt_lons[i] = np.mean(lons[i-left_w:i+right_w])
            filt_ve[i] = np.mean(corr_ve[i-left_w:i+right_w])
            filt_vn[i] = np.mean(corr_vn[i-left_w:i+right_w])
        df.loc[indices, 'filt_lat'] = filt_lats
        df.loc[indices, 'filt_lon'] = filt_lons
        df.loc[indices, 'filt_ve'] = filt_ve
        df.loc[indices, 'filt_vn'] = filt_vn
    return df


def compute_mean_vel(df, box_size=5):
    df[df.columns[0]].count()
    lon_range = np.arange(np.min(df['filt_lon'].to_numpy()), np.max(df['filt_lon'].to_numpy()), box_size)
    lat_range = np.arange(np.min(df['filt_lat'].to_numpy()), np.max(df['filt_lat'].to_numpy()), box_size)
    mean_vn = np.zeros((len(lon_range),len(lat_range)))
    mean_ve = np.zeros((len(lon_range),len(lat_range)))
    # print(df)
    # t_steps = df['hrs'].unique()
    # for t in t_steps:
    #     indices = df.index[df["hrs"]==t]
    print('lon_range:', lon_range, 'lat_range:', lat_range)
    for i, lon in enumerate(lon_range):
        for j, lat in enumerate(lat_range):
            print(i, j)
            box = df.loc[(df['filt_lon']>=lon) & (df['filt_lon']<lon+5) & (df['filt_lat']>=lat) & (df['filt_lat']<lat+5)]
            mean_vn[i,j] = np.mean(box['filt_vn'].to_numpy())
            mean_ve[i,j] = np.mean(box['filt_ve'].to_numpy())
            print(mean_vn[i, j].dtype, mean_vn[i, j])
            print(lon, lon+5, lat, lat+5)

    return mean_vn, mean_ve


def main():
    fpath = 'drifters/'
    fnames = ['buoydata_1_5000.dat', 'buoydata_5001_10000.dat', 'buoydata_10001_15000.dat','buoydata_15001_dec21.dat']    
    
    headers = ['id', 'mm', 'dd', 'yy', 'lat', 'lon', 'temp', 've', 'vn', 'spd', 'var_lat', 'var_lon', 'var_temp']
    df_d = read_drifters_to_df(fpath, fnames[0], headers)
    df_d = df_d.drop(df_d.columns[[6,10,11,12]], axis=1)
    df_d = add_hours_column(df_d)

    df_d = df_d.head(1000)
    df_d = correct_velocities_wind_slip(df_d)
    df_filt = filter_inertial_motion(df_d)
    mean_vn, mean_ve = compute_mean_vel(df_filt)
    print('*', np.shape(mean_vn), np.shape(mean_ve), mean_vn, mean_ve)


    exit()
    plot_drifter_trajectories(df_filt, 'corr_v', global_extent=True)

    # df_d.reset_index()
    # print(df_d.shape)
    # df_d.to_csv('corr_drifters/test.dat', sep=' ', header=False)
    # hdrs2 = ['index', 'id', 'mm', 'dd', 'yy', 'lat', 'lon', 've', 'vn', 'hrs', 'spd', 'corr_ve', 'corr_vn']
    # df_d_corr = read_drifters_to_df('corr_drifters', 'test.dat', hdrs2)
    # df_d_corr.drop(['index'])

    # df_d_1990 = df_d[df_d.yy == 1990]
    # print(df_d_1990.head(200))

    # v_drifters = get_drifter_vel(df_d, 'v')
    # u_drifters = get_drifter_vel(df_d, 'u')

    
    df_month_list = get_time_step_over_month(df_d, years, months)
    df_time_step = get_time_step(df_d, years, months, days)
    for item in df_time_step:
        plot_time_step(item, direction='v')

    # # years = np.arange(1988,2002)
    # # months = np.arange(1,13)
    # # days = np.arange(1,31,0.25)

if __name__ == '__main__':
    main()
