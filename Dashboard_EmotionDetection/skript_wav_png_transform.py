import librosa
import librosa.display
from pathlib import Path
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import os
#from PyQt5 import QtCore, QtGui, QtWidgets
#os.environ["QT_API"] = "pyqt5" 

def precompute_spectrograms(dpi=50):
    dpi = 50

    PATH_TO_WAVs = Path.cwd() / "www" / "wav_transform" / "untransformed_wavs"
    PATH_TO_PNGs = Path.cwd() / "www" / "current_files" / "audio"
    #print(PATH_TO_WAVs)
    files = Path(PATH_TO_WAVs).glob('*.wav')
    for filename in files:
        print(filename)
        audio_tensor, sr = librosa.load(filename, sr=None)
        spectrogram = librosa.feature.melspectrogram(audio_tensor, sr=sr)
        log_spectrogram = librosa.power_to_db(spectrogram, ref=np.max)
        librosa.display.specshow(log_spectrogram, sr=sr, x_axis='time', y_axis='mel')
        plt.gcf().savefig(os.path.join(PATH_TO_PNGs,filename.name) + ".png", dpi=dpi)
#
#     #"surprise" wird von 8 auf 0 geändert
    counter = 0
    path = Path.cwd() / "www" / "current_files" / "audio"
    for file in os.listdir(path):
        #print(file)
        if file.endswith("png"):
            if file.find("transformed_png") > -1:
                counter = counter + 1
                os.rename(os.path.join(path, file), os.path.join(path, file.replace("-08-", "-00-")))
    if counter == 0:
        print("No file has been found")
# # 
# # #precompute_spectrograms(PATH_TO_WAVs)

