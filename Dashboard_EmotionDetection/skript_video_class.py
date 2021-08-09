# -*- coding: utf-8 -*-
"""
Created on Sat Dec  5 11:05:46 2020

@author: Marc
"""

# Imports here
import numpy as np
import pandas as pd
pd.set_option('display.expand_frame_repr', False)
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
from tensorflow.keras.models import Sequential, load_model, save_model
import torch
import json
import os
from torch import nn
from torch import optim
from IPython.display import display
from PIL import Image
from torchvision import datasets, transforms, models
from collections import OrderedDict
#from os import walk
from pathlib import Path
from torch.utils.data import Dataset, DataLoader
  
def check_img(path):

  PATH_TO_FILE = Path.cwd() / path
  PATH_TO_FILE="www/sample/video"
  files = os.listdir(PATH_TO_FILE)

  classification = []
  percentages_0 = []
  percentages_1 = []
  percentages_2 = []
  percentages_3 = []
  percentages_4 = []
  percentages_5 = []


  model = tf.keras.models.load_model("video_model.h5")

  image_list=os.listdir(PATH_TO_FILE)

  for i in range(len(image_list)):
    #PATH_TO_IMAGE = PATH_TO_FILE / image_list[i]
    PATH_TO_IMAGE=PATH_TO_FILE +"/"+ image_list[i]
    print(PATH_TO_IMAGE)
    image_list[i]=keras.preprocessing.image.load_img(PATH_TO_IMAGE, target_size=(150, 150))
  class_names = ['angry', 'fear', 'happy', 'neutral', 'sad', 'surprise']
  result_list = []
  for i in range(0, len(image_list)):

    img_array = keras.preprocessing.image.img_to_array(image_list[i])
    img_array = tf.expand_dims(img_array, 0)
    predictions = model.predict(img_array)
    score = tf.nn.softmax(predictions[0])
    result = []
    for i in range(0, 6):
      result.append(float(score[i]))
    result_list.append(result)
  #return result_list

  for i in range(0, len(result_list)):
    classification.extend([class_names[np.argmax(result_list[i])]])
    percentages_0.extend([str(round(result_list[i][0],3))])
    percentages_1.extend([str(round(result_list[i][1],3))])
    percentages_2.extend([str(round(result_list[i][2],3))])
    percentages_3.extend([str(round(result_list[i][3],3))])
    percentages_4.extend([str(round(result_list[i][4],3))])
    percentages_5.extend([str(round(result_list[i][5],3))])


    

  data = {'file': files, 'classification': classification, 'angry': percentages_0,'happy': percentages_1,
  'fear': percentages_2,'neutral': percentages_3,'sad': percentages_4,'surprise': percentages_5}
  df_results = pd.DataFrame(data=data)
  return df_results

  
