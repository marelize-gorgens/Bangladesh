import numpy as np


class Config:
    geolevel = 'districts'
    tune_min_sample = np.arange(1, 10, 1)
    tune_min_cluster = np.arange(2, 50, 1)
    f_min_sample = 9
    f_min_cluster = 9
