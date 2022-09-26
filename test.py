import os
import pandas as pd
import matplotlib.pyplot as plt


def read_dat_file_crs(path, filename):
    print('2')
    filepath = os.path.join(os.path.normpath(path), filename)
    fid = open(filepath, mode='r')
    df = pd.read_csv(fid, sep="\s+", header=None)
    headers = ['id', 'mm', 'dd', 'yy', 'lat', 'lon', 'temp', 've', 'vn', 'spd', 'var_lat', 'var_lon', 'var_temp']
    df.columns = headers
    df = df.drop(df.columns[[6,10,11,12]], axis=1)
    ids = df['id'].unique()

    for id in ids:
        df_id = df.loc[df['id']==id]
        # print(df_id)
        # drop first and last where speed = 999.999
        df_id = df_id[1:-1]
        # df_id[1:-1].plot.scatter(x = 'lon', y ='lat', c='spd', s=5, colormap='jet', figsize=(20,12))
        # plt.show()

        fig = plt.figure(figsize=(20,12))
        ax = fig.add_subplot(1, 1, 1)
        # ax.add_feature(GSHHSFeature(scale='intermediate', facecolor='lightgrey', linewidth=0.2))
        ax.gridlines()
        plt.scatter(x=df_id.lon, y=df_id.lat, s=5)
        # ax.plot(arr[5], arr[4])
        plt.show()


def main():
    fpath = 'D:/drifters'
    fnames = ['buoydata_1_5000.dat', 'buoydata_5001_10000.dat', 'buoydata_10001_15000.dat','buoydata_15001_dec21.dat']
    # for fname in fnames:
        # read_dat_file(fpath, fname)
    # read_dat_file(fpath, fnames[0])
    read_dat_file_crs(fpath, fnames[0])

if __name__ == '__main__':
    main()
