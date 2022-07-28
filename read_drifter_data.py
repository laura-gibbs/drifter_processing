import os
import math
import numpy as np
import pandas as pd

def read_dat_file(path, filename):
    filepath = os.path.join(os.path.normpath(path), filename)
    fid = open(filepath, mode='r')
    df = pd.read_csv(fid, sep="\s+", header=None)
    headers = ['id', 'mm', 'dd', 'yy', 'lat', 'lon', 'temp', 've', 'vn', 'spd', 'var_lat', 'var_lon', 'var_temp']
    df.columns = headers
    # print(df['id'].nunique())
    ids = df['id'].unique()
    # print(df['id'].value_counts())
    unique_drifters = []
    for id in ids:
        x = df.drop(df.columns[[6,10,11,12]], axis=1)
        # print(x)
        x = x.loc[x['id']==id]
        # print(x)
        # x = x.drop('var_lat', axis=1)
        unique_drifters.append(np.array(x))
        print(x)
    print(len(unique_drifters))
    print(np.shape(unique_drifters[1]))
    print(unique_drifters[0])

def main():
    fpath = 'D:/drifters'
    j, k = 1, 5000
    fnames = []
    for i in range(1,4):
        fname = f'buoydata_{j}_{k*i}'
        j = k*i+1
        if j == 15001:
            fname = f'buoydata_{j}_dec21'
        fnames.append(fname)
        print(fname)
    exit()

    read_dat_file(fpath, fname)

if __name__ == '__main__':
    main()
