import os
import numpy as np
import pandas as pd
import netCDF4 as nc
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
from cartopy.feature import GSHHSFeature
from cartopy.mpl.ticker import LongitudeFormatter, LatitudeFormatter
from scipy import interpolate
from datetime import date
import math
import time
import cmath

omega = 7.2921e-5

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
    df = df.copy()
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
    df = df.copy()
    df_list = []
    for year in years:
        for month in months:
            # df = df.loc[df["spd"]<999.999]
            df_t = df.loc[(df['yy']==year) & (df['mm']==month)]
            df_list.append(df_t)
    return df_list


def get_drifter_vel(df, direction=None):
    df = df.copy()
    # df = df.loc[df["spd"]<999.999]
    if direction == 'v':
        return df['vn']
    elif direction == 'u':
        return df['ve']
    else:
        return df['spd']


def add_hours_column(df):
    df = df.copy()
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
    print('Loading drifter data to dataframe')
    df = read_dat_file(path, filename)
    df.columns = headers
    df.astype(float)
    df = df.loc[df["spd"]<999.999]
    return df


def correct_velocities_wind_slip(df):
    print('Correcting for wind slip')
    df = df.copy()
    df['corr_ve'] = 0
    df['corr_vn'] = 0
    df['wind_u10'] = 0
    df['wind_v10'] = 0
    yrs = sorted(df['yy'].unique())
    print(yrs)
    for yr in yrs:
        print(yr)
        fpath = f'D:/era5/download_{yr}.nc'
        if os.path.exists(fpath):
            print(fpath)
            wind_dataset = nc.Dataset(fpath)
            df = subtract_windslip(df, wind_dataset)
        else:
            print(f'{yr} era5 file does not exist')
    return df


def subtract_windslip(df, wind_dataset):
    df = df.copy()
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
        df.loc[indices, 'wind_u10'] = u10_new
        df.loc[indices, 'wind_v10'] = v10_new
    return df


def filter_inertial_motion(df):
    print('Filtering inertial motion')
    df = df.copy()
    ids = df['id'].unique()

    for i, id in enumerate(ids):
        print(f'{i}/{len(ids)}')
        df_id = df.loc[df['id']==id]
        indices = df.index[df["id"]==id]

        corr_vn = np.array(df_id['corr_vn'])
        corr_ve = np.array(df_id['corr_ve'])
        lons = np.array(df_id['lon'])
        lats = np.array(df_id['lat'])
        lats[lats == 0] = 0.01
        n = len(lats)
        f = 2*omega*np.sin(lats)
        T = (2*np.pi/(f))/3600
        w = (np.floor(np.abs(T/6))).astype(int)

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


def compute_anom_vel_of_box(df, box_size=5):
    print('Computing vn_anom and ve_anom from 5 degree boxes')
    df = df.copy()
    df[df.columns[0]].count()
    df['vn_anom'] = 0
    df['ve_anom'] = 0
    lon_range = np.arange(0, 360, box_size)
    lat_range = np.arange(-90, 90, box_size)
    for lon in lon_range:
        for lat in lat_range:
            indices = (df['filt_lon']>=lon) & (df['filt_lon']<lon+5) & (df['filt_lat']>=lat) & (df['filt_lat']<lat+5)
            box = df.loc[indices]
            if not box.empty:
                mean_vn = np.mean(box['filt_vn'].to_numpy())
                mean_ve = np.mean(box['filt_ve'].to_numpy())
                # print('>>>>', mean_vn, mean_ve)
                df.loc[indices, 'vn_anom'] = box['filt_vn'] - mean_vn
                df.loc[indices, 've_anom'] = box['filt_ve'] - mean_ve
            box = df.loc[indices]
            # print('**', box)
    return df


def compute_norm_wind_stress(df):
    print('Computing normalised wind stress')
    df = df.copy()
    c_d_u = 0.0027 * 1/df['wind_u10'] + 0.000142 + 0.0000764 * df['wind_u10'].abs()
    c_d_v = 0.0027 * 1/df['wind_v10'] + 0.000142 + 0.0000764 * df['wind_v10'].abs()
    rho = 1.225
    tau_u10 = c_d_u * rho * df['wind_u10'] * df['wind_u10'].abs()
    tau_v10 = c_d_v * rho * df['wind_v10'] * df['wind_v10'].abs()
    df['tau_u10'] = tau_u10
    df['tau_v10'] = tau_v10
    f = 2*omega*np.sin(df['filt_lat'])
    df['norm_tu10'] = tau_u10/((f*tau_u10).abs())**(1/2)
    df['norm_tv10'] = tau_v10/((f*tau_v10).abs())**(1/2)
    return df


def compute_anom_wind_stress(df, box_size=5):
    print('Computing anomalies of normalised wind stress')
    df = df.copy()
    df['tu10_anom'] = 0
    df['tv10_anom'] = 0
    lon_range = np.arange(0, 360, box_size)
    lat_range = np.arange(-90, 90, box_size)
    print('lon_range:', lon_range, 'lat_range:', lat_range)
    
    for lon in lon_range:
        for lat in lat_range:
            indices = (df['filt_lon']>=lon) & (df['filt_lon']<lon+5) & (df['filt_lat']>=lat) & (df['filt_lat']<lat+5)
            box = df.loc[indices]
            if not box.empty:
                mean_tu = np.mean(box['norm_tu10'].to_numpy())
                mean_tv = np.mean(box['norm_tv10'].to_numpy())
                df.loc[indices, 'tu10_anom'] = box['norm_tu10'] - mean_tu
                df.loc[indices, 'tv10_anom'] = box['norm_tv10'] - mean_tv
    return df


def load_ssh_data(df):
    print('Loading daily SSH data')
    df = df.copy()
    df['sla'] = 0
    mnths = sorted(df['mm'].unique())
    days = sorted(df['dd'].unique())
    yrs = np.arange(1993,2022)

    for yr in yrs:
        for mnth in mnths:
            for day in days:
                fpath = f'D:/ssh_daily/ssh_{yr}/dt_global_twosat_phy_l4_{yr}{str(mnth).zfill(2)}{str(round(day)).zfill(2)}_vDT2021.nc'
                if os.path.exists(fpath):
                    ssh_dataset = nc.Dataset(fpath)
                    sla = ssh_dataset['sla'][0]
                    lons, lats = ssh_dataset['longitude'], ssh_dataset['latitude']
                    indices = (df['yy']==yr) & (df['mm']==mnth) & (df['dd']==day)
                    df_t = df.loc[indices]
                    f = interpolate.interp2d(lons, lats, sla, kind='cubic')
                    
                    ssh_new = df_t.apply(lambda x: f(x['lon'], x['lat'])[0], axis=1)
                    df.loc[indices,'sla'] = ssh_new
                        
                else:
                    print(f'yr: {yr}, mnth: {mnth}, day: {day} ssh file does not exist')
    return df


def subtract_sla(df):
    print('Subtracting SLA from velocity anomalies')
    df = df.copy()
    df['u_ekman'] = df['ve_anom'] - df['sla']
    df['v_ekman'] = df['vn_anom'] - df['sla']
    return df


def cmplx_least_squares(df, box_size=5):
    print('Lease squares fitting')
    df = df.copy()
    lon_range = np.arange(0, 360, box_size)
    lat_range = np.arange(-90, 90, box_size)
    print('lon_range:', lon_range, 'lat_range:', lat_range)

    b_theta = np.zeros((len(lon_range), len(lat_range)), dtype=np.cdouble)
    for i, lon in enumerate(lon_range):
        print(f'{i}/{len(i)}')
        start = time.time()
        # print(time.asctime(time.localtime(start)), 'i:', i)
        for j, lat in enumerate(lat_range):
            # print('i, j:', i, j)
            indices = (df['filt_lon']>=lon) & (df['filt_lon']<lon+5) & (df['filt_lat']>=lat) & (df['filt_lat']<lat+5)
            box = df.loc[indices]
            if box[box.columns[0]].count()>=1000:
                u_y = box['u_ekman']
                v_y = box['v_ekman']
                u_x = box['tu10_anom']
                v_x = box['tv10_anom']
                y = u_y + 1j * v_y
                x = u_x + 1j * v_x
                c = np.sum(np.conj(x)*y) / np.sum(x**2)
#                 b = cmath.polar(c)[0]
#                 theta = cmath.polar(c)[1]
#                 c = cmath.polar(c)
                b_theta[i,j] = c
        # end = time.time()
        # print('loop time: ', time.asctime(time.localtime(end - start)))
#             else:
#                 for boxes less than 1000
    return b_theta


def drop_nan_rows(df):
    df = df.copy()
    nan_ind = np.where(df['filt_lat'].isna())[0]
    df = df.drop(df.index[[nan_ind]])
    return df

def main():
    fpath = 'drifters/'
    fnames = ['buoydata_1_5000.dat', 'buoydata_5001_10000.dat', 'buoydata_10001_15000.dat','buoydata_15001_dec21.dat']    
    
    compute_intitial = False
    to_csv = True
    plot = True

    if compute_intitial:
        headers = ['id', 'mm', 'dd', 'yy', 'lat', 'lon', 'temp', 've', 'vn', 'spd', 'var_lat', 'var_lon', 'var_temp']
        df_d = read_drifters_to_df(fpath, fnames[0], headers)
        df_d = df_d.drop(df_d.columns[[6,10,11,12]], axis=1)
        df_d = add_hours_column(df_d)
        df_d = df_d[df_d['yy']>=1993]
        df_d = correct_velocities_wind_slip(df_d)
        df_filt = filter_inertial_motion(df_d)
        df_filt = drop_nan_rows(df_filt)
        df_filt_anom = compute_anom_vel_of_box(df_filt)
        df_filt_anom_tau = compute_norm_wind_stress(df_filt_anom)
        df_cmplt = load_ssh_data(df_filt_anom_tau)
        df_cmplt_ekman = subtract_sla(df_cmplt)
        if to_csv:
            df_d.to_csv('tmp/tmp_df_d.csv')
            df_filt.to_csv('tmp/tmp_df_filt.csv')
            df_filt_anom.to_csv('tmp/tmp_df_filt_anom.csv')
            df_filt_anom_tau.to_csv('tmp/tmp_df_filt_anom_tau.csv')
            df_cmplt.to_csv('tmp/tmp_df_cmplt.csv')
            df_cmplt_ekman.to_csv('tmp/tmp_df_cmplt_ekman.csv')
    else:
        df_cmplt_ekman = pd.read_csv('tmp1/tmp_df_cmplt_ekman.csv')
    

    b_theta = cmplx_least_squares(df_cmplt_ekman)
    pd.DataFrame(b_theta).to_csv('b_theta.csv')

    if plot:
        plot_drifter_trajectories(df_cmplt_ekman, 'corr_v', global_extent=True)
        
        yrs = int(sorted(df_cmplt_ekman['yy'].unique()))
        mnths = int(sorted(df_cmplt_ekman['mm'].unique()))
        days = float(sorted(df_cmplt_ekman['dd'].unique()))

        df_month_list = get_time_step_over_month(df_cmplt_ekman, yrs, mnths)
        df_time_step = get_time_step(df_cmplt_ekman, yrs, mnths, days)
        for item in df_time_step:
            plot_time_step(item, direction='v')


if __name__=='__main__':
    main()