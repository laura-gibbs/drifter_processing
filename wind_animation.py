import netCDF4 as nc
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
from cartopy.feature import GSHHSFeature
from cartopy.mpl.ticker import LongitudeFormatter, LatitudeFormatter
import numpy as np
import matplotlib.animation as manimation

def main():
    year = 1994
    fname = f'D:/era5/download_{year}.nc'
    f = nc.Dataset(fname)
    
    time = f.variables['time'][:]
    u10, v10 = f.variables['u10'], f.variables['v10']
    lats, lons = f.variables['latitude'][:], f.variables['longitude'][:]
    FFMpegWriter = manimation.writers['ffmpeg']
    metadata = dict(title='ERA 5', artist='Matplotlib',
                    comment='-comment-')
    writer = FFMpegWriter(fps=9, metadata=metadata)

    fig = plt.figure(figsize=(20,10))
    fig.subplots_adjust(left=0.035, bottom=0.01, right=0.94, top=0.99, wspace=None, hspace=None)
    crs = ccrs.PlateCarree()
    ax = fig.add_subplot(1, 1, 1, projection=crs)
    ax.add_feature(GSHHSFeature(facecolor='none', linewidth=0.3))
    ax.set_yticks(np.linspace(-90, 90, 9), crs=crs)
    lon_formatter = LongitudeFormatter(zero_direction_label=True)
    lat_formatter = LatitudeFormatter()
    ax.xaxis.set_major_formatter(lon_formatter)
    ax.yaxis.set_major_formatter(lat_formatter)
    ax.set_xticks(np.linspace(-180, 180, 19), crs=crs)
    cbar_ax = fig.add_axes([0.955, 0.048, 0.011, 0.905])

    # exit()
    with writer.saving(fig, f"era5_{year}.mp4", 100):
        u10 = u10[:100]
        for i in range(len(u10)):
            print(i)
            t = (time[i] - time[0])/24
            im = ax.pcolormesh(lons, lats, u10[i], transform=crs, cmap='jet', vmin=-25, vmax=25)
            ax.set_title(f'ERA5 6-hourly data (u component): Day {t}')
            fig.colorbar(im, cax=cbar_ax, ticks=np.linspace(-25, 25, num=11))
            writer.grab_frame()


if __name__ == '__main__':
    main()
