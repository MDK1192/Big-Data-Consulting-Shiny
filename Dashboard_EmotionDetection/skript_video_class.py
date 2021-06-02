# -*- coding: utf-8 -*-
"""
Created on Sat Dec  5 11:05:46 2020

@author: Marc
"""

# Imports here
import numpy as np
import pandas as pd
pd.set_option('display.expand_frame_repr', False)

#import matplotlib.pyplot as plt
#%matplotlib inline
#import pathlib

import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
from tensorflow.keras.models import Sequential, load_model, save_model
import torch
import json
import os
from torch import nn
from torch import optim
from PIL import Image
from torchvision import datasets, transforms, models
from collections import OrderedDict
#from os import walk
from pathlib import Path
from torch.utils.data import Dataset, DataLoader
  
def check_img(path):

  PATH_TO_FILE = Path.cwd() / path
  files = os.listdir(PATH_TO_FILE)

  classification = []
  percentages_0 = []
  percentages_1 = []
  percentages_2 = []
  percentages_3 = []
  percentages_4 = []
  percentages_5 = []
  percentages_6 = []
  percentages_7 = []

  model = tf.keras.models.load_model("video_model.h5")

  image_list=os.listdir(PATH_TO_FILE)
  for i in range(len(image_list)):
    PATH_TO_IMAGE = PATH_TO_FILE / image_list[i]
    print(PATH_TO_IMAGE)
    image_list[i]=keras.preprocessing.image.load_img(PATH_TO_IMAGE, target_size=(64, 64))
  class_names = ['angry', 'contempt', 'disgust', 'fear', 'happy', 'neutral', 'sad', 'surprise']
  result_list = []
  for i in range(0, len(image_list)):

    img_array = keras.preprocessing.image.img_to_array(image_list[i])
    img_array = tf.expand_dims(img_array, 0)
    predictions = model.predict(img_array)
    score = tf.nn.softmax(predictions[0])
    result = []
    for i in range(0, 8):
      result.append(float(score[i]))
    result_list.append(result)
  #return result_list

  for i in range(0, len(result_list)):
    classification.extend([class_names[np.argmax(result_list[i])]])
    percentages_0.extend([str(round(result_list[i][0],4))])
    percentages_1.extend([str(round(result_list[i][1],4))])
    percentages_2.extend([str(round(result_list[i][2],4))])
    percentages_3.extend([str(round(result_list[i][3],4))])
    percentages_4.extend([str(round(result_list[i][4],4))])
    percentages_5.extend([str(round(result_list[i][5],4))])
    percentages_6.extend([str(round(result_list[i][6],4))])
    percentages_7.extend([str(round(result_list[i][7],4))])
    

  data = {'file': files, 'classification': classification, 'angry': percentages_0, 'contempt': percentages_1, 'disgust': percentages_2,
  'fear': percentages_3,'happy': percentages_4,
  'neutral': percentages_5,'sad': percentages_6, 'surprise': percentages_7}
  df_results = pd.DataFrame(data=data)
  return df_results

  
