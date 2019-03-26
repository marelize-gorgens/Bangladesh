import sys
import os
import re
import numpy as np
import pandas as pd
from dbfread import DBF
from simpledbf import Dbf5
from rpy2.robjects import pandas2ri
from rpy2.robjects.packages import importr

DATA_DIR = sys.argv[1]
OUT_DIR = sys.argv[2]


def read_dbf(file):
    try:
        df = Dbf5(file)
        df = df.to_dataframe()
    except error:
        df = DBF(file)
        df = pd.DataFrame(iter(df))
    return df


def read_sav(file):
    foreign = importr('foreign')
    pandas2ri.activate()
    df = foreign.read_spss(file, reencode=False)
    tmp_dict = dict()
    for name in list(df.names):
        tm = np.array(df.rx(str(name)))
        tmp_dict[name] = tm.flatten()
    df = pd.DataFrame.from_dict(tmp_dict)
    return df


def read_dta(file_path):
    df = pd.read_stata(file_path)
    return df


def svrs_metadata(data_path, out_path):
    output = {'variable': [], 'variable_definition': [], 'file_name': [], 'year': [], 'file_path': [], 'folder': []}
    for path, subdirs, files in os.walk(data_path):
        files_str = [file for file in files if
                     bool(re.search(".dbf", file)) or
                     bool(re.search(".dta", file)) or
                     bool(re.search(".sav", file))]
        for file in files_str:
            y = "20" + os.path.split(path)[1][-2:]
            if bool(re.search(r".dbf|.DBF", file)):
                df = read_dbf(os.path.join(path, file))
                print("#" * 20 + f" Year: {y} DBF (dBASE) File: {file} " + "#" * 20)
            elif bool(re.search(r".dta|.DTA", file)):
                df = read_dta(os.path.join(path, file))
                print("#" * 20 + f" Year: {y} DTA (STATA) File: {file} " + "#" * 20)
            elif bool(re.search(r".sav|.SAV", file)):
                df = read_sav(os.path.join(path, file))
                print("#" * 20 + f" Year: {y} SAV (SPSS) File: {file} " + "#" * 20)
            else:
                print(f"Check the structure of file {file}!")
            output['variable'] = output['variable'] + list(df.columns)
            output['variable_definition'] = output['variable_definition'] + [" "] * df.shape[1]
            output['file_name'] = output['file_name'] + [file] * df.shape[1]
            output['year'] = output['year'] + ["20" + os.path.split(path)[1][-2:]] * df.shape[1]
            output['folder'] = output['folder'] + [os.path.split(path)[1]] * df.shape[1]
            output['file_path'] = output['file_path'] + [os.path.join(path, file)] * df.shape[1]
    df = pd.DataFrame.from_dict(output)
    df = df.sort_values(by=['year', 'file_name', 'variable'])
    df.to_csv(os.path.join(out_path, 'dp_svrs_metadata.csv'), index=False, index_label=False)


def main():
    svrs_metadata(data_path=DATA_DIR, out_path=OUT_DIR)


if __name__ == "__main__":
    main()
