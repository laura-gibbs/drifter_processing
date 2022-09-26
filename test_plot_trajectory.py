import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def read_dat_file(path, filename):
    filepath = os.path.join(os.path.normpath(path), filename)
    fid = open(filepath, mode='r')
    df = pd.read_csv(fid, sep="\s+", header=None)
    headers = ['id', 'mm', 'dd', 'yy', 'lat', 'lon', 'temp', 've', 'vn', 'spd', 'var_lat', 'var_lon', 'var_temp']
    df.columns = headers
    df = df.drop(df.columns[[6,10,11,12]], axis=1)
    ids = df['id'].unique()
    for id in ids:
        df_id = df.loc[df['id']==id]
        print(df_id)
        # drop first and last where speed = 999.999
        df_id[1:-1].plot.scatter(x = 'lon', y ='lat', c='spd', s=5, colormap='jet', figsize=(20,12))
        plt.show()


def main():
    fpath = 'D:/drifters'
    fnames = ['buoydata_1_5000.dat', 'buoydata_5001_10000.dat', 'buoydata_10001_15000.dat','buoydata_15001_dec21.dat']
    read_dat_file(fpath, fnames[0])

if __name__ == '__main__':
    main()
