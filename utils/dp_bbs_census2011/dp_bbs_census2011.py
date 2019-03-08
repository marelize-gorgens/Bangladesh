import os
import re
import sys
import numpy as np
import pandas as pd

DATA_DIR = sys.argv[1]
OUT_PATH = sys.argv[2]


def collect_filenames(data_dir):
    files_list = []
    for path, subdirs, files in os.walk(data_dir):
        for file in files:
            if bool(re.search(pattern='.csv', string=file)):
                files_list.append(os.path.join(path, file))
    return files_list


def process_files(files_list, out_dir):
    for file in files_list:
        data = pd.read_csv(file, low_memory=False)
        if all([var in list(data.columns) for var in ['UZ', 'UN/WA', 'MZ/MH', 'Vill', 'RMO']]):
            for var in ['UZ', 'UN/WA', 'MZ/MH', 'Vill', 'RMO']:
                if var == 'MZ/MH':
                    data[var] = data[var].astype(str).str.replace('-9', 'XXX')
                    data[var] = data[var].str.pad(width=3, side='left', fillchar='0')
                else:
                    data[var] = data[var].astype(str).str.replace('-9', 'XX')
                    data[var] = data[var].str.pad(width=2, side='left', fillchar='0')
        else:
            for var in ['1', '2', '3', '4', '5']:
                if var == 3:
                    data[var] = data[var].astype(str).str.replace('-9', 'XXX')
                    data[var] = data[var].str.pad(width=3, side='left', fillchar='0')
                else:
                    data[var] = data[var].astype(str).str.replace('-9', 'XX')
                    data[var] = data[var].str.pad(width=2, side='left', fillchar='0')

        check = ['XX', 'XX', 'XXX', 'XX', 'XX']
        check_var = []
        for key, values in enumerate(data.values):
            geos = values[1:6]
            geo_check = [i == j for i, j in zip(geos, check)]
            geo_check = list(np.where(geo_check, 1, 0))
            geo_check = [str(i) for i in geo_check]
            geo_check = ''.join(geo_check)
            check_var.append(geo_check)
        data['check_var'] = check_var
        for subset in data['check_var'].unique():
            if subset == '11111':
                tmp = data[data['check_var'] == subset]
                tmp.to_csv(os.path.join(out_dir, 'ZilaTotal_' + os.path.split(file.replace('.pdf', '.csv'))[1]),
                           index=False, index_label=False)
            elif subset == '11110':
                tmp = data[data['check_var'] == subset]
                tmp_urban = tmp[tmp['RMO'] == '01']
                tmp_urban.to_csv(os.path.join(out_dir, 'ZilaTotal_Urban_' + os.path.split(file.replace('.pdf', '.csv'))[1]),
                                 index=False, index_label=False)
                tmp_rural = tmp[tmp['RMO'] == '02']
                tmp_rural.to_csv(os.path.join(out_dir, 'ZilaTotal_Rural_' + os.path.split(file.replace('.pdf', '.csv'))[1]),
                                 index=False, index_label=False)
                tmp_other = tmp[tmp['RMO'] == '03']
                tmp_other.to_csv(os.path.join(out_dir, 'ZilaTotal_Other_' + os.path.split(file.replace('.pdf', '.csv'))[1]),
                                 index=False, index_label=False)
            elif subset == '01111':
                tmp = data[data['check_var'] == subset]
                tmp.to_csv(os.path.join(out_dir, 'UpazilaTotal_' + os.path.split(file.replace('.pdf', '.csv'))[1]),
                           index=False, index_label=False)
            elif subset == '01110':
                tmp = data[data['check_var'] == subset]
                tmp_urban = tmp[tmp['RMO'] == '01']
                tmp_urban.to_csv(os.path.join(out_dir, 'UpazilaTotal_Urban_' + os.path.split(file.replace('.pdf', '.csv'))[1]),
                                 index=False, index_label=False)
                tmp_rural = tmp[tmp['RMO'] == '02']
                tmp_rural.to_csv(os.path.join(out_dir, 'UpazilaTotal_Rural_' + os.path.split(file.replace('.pdf', '.csv'))[1]),
                                 index=False, index_label=False)
                tmp_other = tmp[tmp['RMO'] == '03']
                tmp_other.to_csv(os.path.join(out_dir, 'UpazilaTotal_Other_' + os.path.split(file.replace('.pdf', '.csv'))[1]),
                                 index=False, index_label=False)
            elif subset == '00111':
                tmp = data[data['check_var'] == subset]
                tmp.to_csv(os.path.join(out_dir, 'UnionWardsTotal_' + os.path.split(file.replace('.pdf', '.csv'))[1]),
                           index=False, index_label=False)
            elif subset == '00110':
                tmp = data[data['check_var'] == subset]
                tmp_urban = tmp[tmp['RMO'] == '01']
                tmp_urban.to_csv(os.path.join(out_dir, 'UnionWardsTotal_Urban_' + os.path.split(file.replace('.pdf', '.csv'))[1]),
                                 index=False, index_label=False)
                tmp_rural = tmp[tmp['RMO'] == '02']
                tmp_rural.to_csv(os.path.join(out_dir, 'UnionWardsTotal_Rural_' + os.path.split(file.replace('.pdf', '.csv'))[1]),
                                 index=False, index_label=False)
                tmp_other = tmp[tmp['RMO'] == '03']
                tmp_other.to_csv(os.path.join(out_dir, 'UnionWardsTotal_Other_' + os.path.split(file.replace('.pdf', '.csv'))[1]),
                                 index=False, index_label=False)
            elif subset == '00010':
                tmp = data[data['check_var'] == subset]
                tmp.to_csv(os.path.join(out_dir, 'MouzaMahallaTotal_' + os.path.split(file.replace('.pdf', '.csv'))[1]),
                           index=False, index_label=False)
            elif subset == '00000':
                tmp = data[data['check_var'] == subset]
                tmp.to_csv(os.path.join(out_dir, 'VillageTotal_' + os.path.split(file.replace('.pdf', '.csv'))[1]),
                           index=False, index_label=False)


def main():
    files = collect_filenames(data_dir=DATA_DIR)
    process_files(files_list=files, out_dir=OUT_PATH)


if __name__ == '__main__':
    main()
