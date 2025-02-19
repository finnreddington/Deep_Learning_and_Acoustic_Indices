

#import packages/modules
from AudiosetAnalysis import AudiosetAnalysis
import pandas as pd
from datetime import timedelta
from datetime import datetime
import os

### Change patsh if you structure folders

# Path to the location where your audio file are stored:
audio_dir = (r'')

# Path to folder containing vggish setup files and 'AudiosetAnalysis' downloaded from sarebs supplementary
#vggish_files = r'/content/drive/MyDrive/Reef soundscapes with AI/Audioset' 

# Output folder for results:
output_folder_1min = r'C:\Reef soundscapes with AI\Results\PCNN_features'

#Navigate to the folder containing setup files, including AudiosetAnalysis downloaded from sarebs supplementary
#os.chdir(vggish_files) 

# Setup the audioset analysis
an = AudiosetAnalysis()
an.setup()

#select files
all_fs = os.listdir(audio_dir) #list of all files in directory
audio_fs = [f for f in all_fs if '.wav' in f.lower() or '.mp3' in f.lower()] #list of all audio files in dir: .wav or .mps

#initiate empty dataframes to save results
results_df_1sec = pd.DataFrame()
results_df_1min = pd.DataFrame() 

# Feature extraction loop
for f in audio_fs:
    """This loop takes the current filename, rips the timestamp, appends the corresponding length of time being analysed
    additively to a new name, calculates VGGish features from each 0.96s chunk, averages these for each 1min file and saves 
    the results to a csv in your GDrive"""
    path = os.path.join(audio_dir, f)
    print(f) #print file name
    
    #extract timestamp from filename - adjust this if using your own naming convention
    t1 = f.split("_")[1][0:5]
    t2 = f.split("_")[2][0:4]
    
    #t2 = f.split(".")[1][0:4]
    #t1 = f.split(".")[3]
    
    t = t1+t2+'00'
    recording_start_time = pd.to_datetime(t, format='%y%m%d%H%M%S') 
    slice_time = recording_start_time - timedelta(milliseconds=960)
    mean_slice_time = recording_start_time - timedelta(minutes=1)
    
    #calculate feature values
    results = an.analyse_audio(path)
    
    # Uncomment for 0.96s results:
    # r1sec = results['raw_audioset_feats_960ms']
    # for count, r1sec in enumerate(r1sec):
    #     slice_time = slice_time + timedelta(milliseconds=960)
    #     string_time = slice_time.strftime('%H.%M.%S.%f')[:-4]
    #     result_name = f[:-4] + 'T' + string_time + '.wav'
    #     #result_name = f[:-4]+'T'+str(count+1)+'.wav' #use this line if not using ST timestamped files
    #     results_df_1sec[result_name] = pd.Series(results['raw_audioset_feats_960ms'][count])
    
    #Save 1min results:
    r1min = results['raw_audioset_feats_59520ms']
    for count, r1min in enumerate(r1min):
        #store the timestamp
        mean_slice_time = mean_slice_time + timedelta(minutes=1)
        string_time = mean_slice_time.strftime('%H.%M.%S.%f')[:-4]
        result_name = f[:-4] + 'T' + string_time + '.wav'
        #result_name = f[:-4]+'T'+str(count+1)+'.wav' #use this line if not using ST timestamped files
        results_df_1min[result_name] = pd.Series(results['raw_audioset_feats_59520ms'][count])


#save a timestamped csv with 1min results
now = datetime.now()
time_now = now.strftime("%H.%M.%S")
results_df_1min.to_csv(output_folder_1min + '/Kimbe_features' + '.csv')
