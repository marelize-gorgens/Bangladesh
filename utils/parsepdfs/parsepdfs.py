import os
import sys
import re
import PyPDF2
import pandas as pd
from config import Config
from collections import Counter

DATA_DIR = sys.argv[1]
OUT_DIR = sys.argv[2]
EXCLUDE = sys.argv[3]


def check_float(string):
    try:
        float(string)
        return True
    except ValueError:
        return False


def get_pdf_files(path):
    output = {'file': [], 'program': []}
    for path, subdirs, files in os.walk(path):
        for name in files:
            if bool(re.search(r'_C[0-9][0-9].pdf', name)):
                output['file'].append(os.path.join(path, name))
                output['program'].append(name[-7:-4])
    return pd.DataFrame.from_dict(output)


def check_pdf(file):
    df_file_obj = open(file, 'rb')
    pdf_reader = PyPDF2.PdfFileReader(df_file_obj, strict=False)
    num_pages = pdf_reader.getNumPages()
    pages = ''
    for page in range(num_pages):
        page_obj = pdf_reader.getPage(page)
        pages = pages + page_obj.extractText()
    pages = pages.split("\n")
    pages = [element.replace(",", "") for element in pages]
    check_points = []
    for key, element in enumerate(pages):
        if element.isspace() or not element.strip():
            pages[key] = float('nan')
        if key > 6:
            try:
                digits_check = pages[key - 4:key + 5]
                string_check = digits_check.pop(4)
                digits_check = [int(digit) for digit in digits_check]
                geoname_check = not string_check.isdigit()
                values_check_int = all([str(digit).isdigit() for digit in digits_check])
                geoname_values_check = geoname_check and values_check_int
                patterns = ['Zila', 'Upazila', 'Ward', 'Union', '*']
                pattern_check = any([pattern in element for pattern in patterns])
                if geoname_values_check or pattern_check:
                    check_points.append(key)
            except ValueError:
                digits_check = pages[key - 4:key + 5]
                string_check = digits_check.pop(4)
                geoname_check = not check_float(string_check)
                values_check_float = all([check_float(digit) for digit in digits_check])
                geoname_values_check = geoname_check and values_check_float
                patterns = ['Zila', 'Upazila', 'Ward', 'Union', '*']
                pattern_check = any([pattern in element for pattern in patterns])
                if geoname_values_check or pattern_check:
                    check_points.append(key)
            else:
                pass
    out = [j - i for i, j in zip(check_points[:-1], check_points[1:])]
    return int(Counter(out).most_common(1)[0][0])


def parse_files(file, ncol_before, ncol_after, col_names):
    df_file_obj = open(file, 'rb')
    pdf_reader = PyPDF2.PdfFileReader(df_file_obj, strict=False)
    num_pages = pdf_reader.getNumPages()
    pages = ''
    subset = {}
    for page in range(num_pages):
        page_obj = pdf_reader.getPage(page)
        pages = pages + page_obj.extractText()
    pages = pages.split("\n")
    pages = [element.replace(",", "") for element in pages]
    for key, element in enumerate(pages):
        if element.isspace() or not element.strip():
            pages[key] = -9
        if key > 6:
            try:
                digits_check = pages[key - 4:key + 5]
                string_check = digits_check.pop(4)
                digits_check = [int(digit) for digit in digits_check]
                geoname_check = not string_check.isdigit()
                values_check_int = all([str(digit).isdigit() for digit in digits_check])
                geoname_values_check = geoname_check and values_check_int
                patterns = ['Zila', 'Upazila', 'Ward', 'Union', '*']
                pattern_check = any([pattern in element for pattern in patterns])
                if geoname_values_check or pattern_check:
                    subset[key] = pages[key - ncol_before:key + ncol_after]
            except (ValueError, AttributeError):
                digits_check = pages[key - 4:key + 5]
                string_check = digits_check.pop(4)
                geoname_check = not check_float(string_check)
                values_check_float = all([check_float(digit) for digit in digits_check])
                geoname_values_check = geoname_check and values_check_float
                patterns = ['Zila', 'Upazila', 'Ward', 'Union', '*']
                pattern_check = any([pattern in element for pattern in patterns])
                if geoname_values_check or pattern_check:
                    subset[key] = pages[key - ncol_before:key + ncol_after]
            else:
                pass
    if col_names is None:
        subset = pd.DataFrame.from_dict(subset, orient='index')
    else:
        subset = pd.DataFrame.from_dict(subset, orient='index', columns=col_names)
    df_file_obj.close()
    return subset


def write_data(input_files, output_dir, exclude=None):
    programs = list(input_files['program'].unique())
    for subset in sorted(programs, reverse=True):
        if not (subset == exclude):
            tmp = input_files[input_files['program'] == subset]
            out_list = []
            for key, values in enumerate(tmp.values):
                print(f"Parsing {values[0]} file")
                data = parse_files(values[0],
                                   ncol_before=Config.parse_parameters[values[1]]['ncol_before'],
                                   ncol_after=Config.parse_parameters[values[1]]['ncol_after'],
                                   col_names=Config.cols_dict[values[1]])
                out_list.append(data)
            program_df = pd.concat(out_list)
            print(f"Concatenating parsed data from program for all geographies: {subset} ")
            program_df.to_csv(os.path.join(output_dir, subset + ".csv"), index=False, index_label=None)
            print(f"Writing parsed data from program for all geographies: {subset} ")
        else:
            tmp = input_files[input_files['program'] == subset]
            os.system(f"mkdir {os.path.join(output_dir)}")
            os.system(f"mkdir {os.path.join(output_dir, exclude)}")
            for key, values in enumerate(tmp.values):
                print(f"Parsing {values[0]} file")
                num_cols = check_pdf(values[0])
                data = parse_files(values[0],
                                   ncol_before=6,
                                   ncol_after=num_cols - 6,
                                   col_names=None)
                print(f"Writing individual excluded files  from program for all geographies: {subset} ")
                data.to_csv(os.path.join(os.path.join(output_dir, exclude),
                                         os.path.split(values[0])[1].replace(".pdf", ".csv")),
                            index=False, index_label=None)
                # data = parse_files(values[0],
                #                    ncol_before=Config.parse_parameters[values[1]]['ncol_before'],
                #                    ncol_after=Config.parse_parameters[values[1]]['ncol_after'],
                #                    col_names=Config.cols_dict[values[1]])


def main():
    print(f"Getting all corresponding pdf files from {DATA_DIR}")
    out = get_pdf_files(path=DATA_DIR)
    write_data(input_files=out, output_dir=OUT_DIR, exclude=EXCLUDE)
    return out


if __name__ == '__main__':
    main()
