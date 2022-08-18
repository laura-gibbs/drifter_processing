import netCDF4 as nc
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
from cartopy.feature import GSHHSFeature
from cartopy.mpl.ticker import LongitudeFormatter, LatitudeFormatter
import numpy as np
import matplotlib.animation as manimation


def read_surface(filename):
    fid = open(filename, mode='rb')
    buffer = fid.read(4)
    fid.seek(0)
    floats = np.array(np.frombuffer(fid.read(), dtype=np.float32), order='F')
    floats = floats[1:len(floats)-1]
    floats = np.reshape(floats, (1440,720), order='F')
    floats[floats <= -1.7e7] = np.nan
    return np.rot90(floats, 1)


def main():
    FFMpegWriter = manimation.writers['ffmpeg']
    # exit()
    metadata = dict(title='Movie Test', artist='Matplotlib',
                    comment='comment')
    writer = FFMpegWriter(fps=9, metadata=metadata)
    
    fig = plt.figure(figsize=(20,10))
    fig.subplots_adjust(left=0.035, bottom=0.01, right=0.94, top=0.99, wspace=None, hspace=None)
    crs = ccrs.PlateCarree()
    ax = fig.add_subplot(1, 1, 1, projection=crs)
    ax.add_feature(GSHHSFeature(facecolor='none', linewidth=0.3))
    ax.set_yticks(np.linspace(-90, 90, 9), crs=crs)
    lon_formatter = LongitudeFormatter()#zero_direction_label=True)
    lat_formatter = LatitudeFormatter()
    ax.xaxis.set_major_formatter(lon_formatter)
    ax.yaxis.set_major_formatter(lat_formatter)
    ax.set_xticks(np.linspace(-180, 180, 19), crs=crs)
    # left_pos, bottom_pos, cbarwidth, cbarheight])
    cbar_ax = fig.add_axes([0.955, 0.048, 0.011, 0.905])

    start, end = 1993, 2021
    years = np.arange(start, end+1)
    mnths = ['01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12']
    # mss = read_surface('C:/Users/oa18724/Documents/Master_PhD_folder/a_mdt_data/computations/mss/dtu18_do0330_rr0004.dat')
    with writer.saving(fig, f"sla_{start}_{end}.mp4", 100):
        for year in years:
            for mnth in mnths:
                print(f'year:{year}, month:{mnth}')
                fname = f'D:/adt_calculations/SSH/duacs/cmems_obs-sl_glo_phy-ssh_my_allsat-l4-duacs-0.25deg_P1M-m/{year}/dt_global_allsat_msla_h_y{year}_m{mnth}.nc'
                f = nc.Dataset(fname)
                lats, lons = f.variables['latitude'][:], f.variables['longitude'][:]
                time = f.variables['time']
                sla = f.variables['sla']
                im = ax.pcolormesh(lons, lats, sla[0], transform=crs, cmap='jet', vmin=-0.3, vmax=0.3)
                # cbar_ax.tick_params(labelsize=12)
                fig.colorbar(im, cax=cbar_ax, ticks=np.linspace(-0.3, 0.3, num=7))
                ax.set_title(f'SLA DUACS: {year}, month {mnth}')
                # plt.show()
                writer.grab_frame()

if __name__ == '__main__':
    main()
