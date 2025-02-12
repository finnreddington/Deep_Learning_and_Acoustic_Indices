
# in terminal: pip install scikit-maad

import pandas as pd
import os
from maad import sound, features, util
from maad.util import (date_parser, plot_correlation_map, plot_features_map, plot_features, false_Color_Spectro)

from statistics import mean
from statistics import stdev
import numpy as np 

# may be some redundant imports
from datetime import datetime
from datetime import timedelta
from datetime import time
from time import strftime
import time

# Path to the location where your audio file are stored:
audio_dir = date_parser(r"D:\audiodir_test")

results_directory = r'C:\Users\fr15610\OneDrive - University of Bristol\Desktop\soundscape_AI\Reef soundscapes with AI\Results\PCNN_features'

# define the function to extract features

def index_loops(audio_files, filenames, slice_names, NDSI_fullband,
                #0.96s values
                fish_ACI, fish_ADI, fish_BI, fish_H, fish_Ht, fish_Hf, fish_M,
                shrimp_ACI, shrimp_ADI, shrimp_BI, shrimp_H, shrimp_Ht, shrimp_Hf, shrimp_M,
                full_ACI, full_ADI, full_BI, full_H, full_Ht, full_Hf, full_M,
                #1min averaged values
                fish_ACI_1min_mean, fish_ADI_1min_mean, fish_BI_1min_mean, fish_H_1min_mean,
                fish_Ht_1min_mean, fish_Hf_1min_mean, fish_M_1min_mean,
                shrimp_ACI_1min_mean, shrimp_ADI_1min_mean, shrimp_BI_1min_mean, shrimp_H_1min_mean,
                shrimp_Ht_1min_mean, shrimp_Hf_1min_mean, shrimp_M_1min_mean,
                full_ACI_1min_mean, full_ADI_1min_mean, full_BI_1min_mean, full_H_1min_mean,
                full_Ht_1min_mean, full_Hf_1min_mean, full_M_1min_mean,
                NDSI_1min_mean,
                fish_ACI_1min_std, fish_ADI_1min_std, fish_BI_1min_std, fish_H_1min_std,
                fish_Ht_1min_std, fish_Hf_1min_std, fish_M_1min_std,
                shrimp_ACI_1min_std, shrimp_ADI_1min_std, shrimp_BI_1min_std, shrimp_H_1min_std,
                shrimp_Ht_1min_std, shrimp_Hf_1min_std, shrimp_M_1min_std,
                full_ACI_1min_std, full_ADI_1min_std, full_BI_1min_std, full_H_1min_std,
                full_Ht_1min_std, full_Hf_1min_std, full_M_1min_std,
                NDSI_1min_std,
                mean_slice_names):
    
    df = audio_files

    #create lists that will temporarily store index results from which the mean can be calculated, then reset
    fish_ACI_temp = []
    fish_ADI_temp = []
    fish_BI_temp = []
    fish_H_temp = []
    fish_Ht_temp = []
    fish_Hf_temp = []
    fish_M_temp = []

    shrimp_ACI_temp = []
    shrimp_ADI_temp = []
    shrimp_BI_temp = []
    shrimp_H_temp = []
    shrimp_Ht_temp = [] 
    shrimp_Hf_temp = []
    shrimp_M_temp = []
    
    full_ACI_temp = []
    full_ADI_temp = []
    full_BI_temp = []
    full_H_temp = []
    full_Ht_temp = []
    full_Hf_temp = []
    full_M_temp = []

    NDSI_temp = []
    
    for index, row in df.iterrows() :
        """ The following loop completes these tasks: load audio file, slice into 0.96s segments, append the time to each 
        slice with 0.96s added to end of the file name each slice, filter each segment into fish & shrimp band, 
        compute the spectrogram for indices that require this, compute all indices for each segment in both bands, 
        append the results to lists which are combined into a dataframe after the loop"""

        # get the full filename of the corresponding row
        fullfilename = row['file']
        # Save file basename
        path, filename = os.path.split(fullfilename)

        #add file name to list for results dataframe
        filenames.append(filename)

        #extract timestamp from filename - adjust this if using your own naming convention
        t1 = filename.split("_")[1][0:5]
        t2 = filename.split("_")[2][0:4]
        #t2 = f.split(".")[1][0:4]
        #t1 = f.split(".")[3]
        t = t1+t2+'00'
        recording_start_time = pd.to_datetime(t, format='%y%m%d%H%M%S') 
        slice_time = recording_start_time - timedelta(milliseconds=960)
        mean_slice_time = recording_start_time - timedelta(minutes=1)

        #print file name being iterated upon
        print ('\n**************************************************************')
        print (filename)

        #### Load the original sound (16bits) and get the sampling frequency fs
        try:
            wave, fs = sound.load(filename=fullfilename, channel='left', detrend=True, verbose=False)
        except:
            # Delete the row if the file does not exist or raise a value error (i.e. no EOF)
            df.drop(index, inplace=True)
            continue

        #divide length of audio file by 0.96s = total_segs, round down to nearest integer, then do a for loop that goes from
         #0:total_segs. This loop should do the index to each 

        #get the length of audio file in seconds using number of samples/sample rate
        total_length = len(wave)/fs

        # get number of 0.96s segs in file, rounding down to nearest integer so there is no partial seg at the end
        number_segs = int(total_length/0.96) #round

        # 0.96s time keeping
        start = 0
        end = 0.96
        zero_time = recording_start_time - timedelta(milliseconds=960) #zero_time - timedelta(milliseconds=960) #so that the first iteration resets to 0

        #counter for averaging
        count = 0 

        for k in range (number_segs): #should this be number_segs-1? #don't think so
            sliced_file = sound.trim(wave, fs, min_t = start, max_t = end)
            start = round(start + 0.96, 2)
            end = round(end + 0.96, 2)

            #naming of slices using time   ##### Make some changes here to make te 0.96s segments names neater
            zero_time = zero_time + timedelta(milliseconds=960)
            string_time = zero_time.strftime('%H.%M.%S.%f')[:-4]
            track_name = filename[:-4] + 'T' + string_time + '.wav'
            slice_names.append(track_name) 


            ####### fish band
            #filter track
            filtered = sound.select_bandwidth(x = sliced_file, fs = fs, fcut = [50, 2000], forder = 5,
                                fname = 'butter', ftype = 'bandpass')

            #compute spectrogram for: ACI, ADI, Hf
            Sxx, tn, fn, ext = sound.spectrogram (filtered, fs, nperseg = 256)  

            #ACI
            _, _ , ACI_fish = features.acoustic_complexity_index(Sxx)

            #ADI, uses bins 10 bins, each 1/10th of the full freq range
            ADI_fish = features.acoustic_diversity_index(Sxx, fn, fmin = 50, fmax = 2000, bin_step = 190, dB_threshold = -50)

            #BI
            BI_fish = features.bioacoustics_index(Sxx, fn, flim = (50, 2000))

            #H
            H_fish = util.entropy(filtered)

            #Ht
            #Ht_fish = features.temporal_entropy(filtered, Nt = 256)

            #Hf
            Hf_fish, Ht_per_bin = features.frequency_entropy(Sxx)

            #M, amplitude index
            M_fish = features.temporal_median(filtered, Nt = 256)

            #append results to lists created earlier
            fish_ACI.append(ACI_fish) 
            fish_ADI.append(ADI_fish) 
            fish_BI.append(BI_fish)
            fish_H.append(H_fish) 
            #fish_Ht.append(Ht) 
            fish_Hf.append(Hf_fish)
            fish_M.append(M_fish) 




            ####### shrimp band
            #filter track
            filtered = sound.select_bandwidth(x = sliced_file, fs = fs, fcut = [2000, 8000], forder = 5,
                                    fname = 'butter', ftype = 'bandpass')

            #compute spectrogram for: ACI, ADI, Hf
            Sxx, tn, fn, ext = sound.spectrogram (filtered, fs, nperseg = 256)  

            #ACI
            _, _ , ACI_shrimp = features.acoustic_complexity_index(Sxx)

            #ADI, uses bins 10 bins, each 1/10th of the full freq range
            ADI_shrimp = features.acoustic_diversity_index(Sxx,fn, fmin = 2000, fmax = 8000, bin_step = 600, dB_threshold = -50)

            #BI
            BI_shrimp = features.bioacoustics_index(Sxx,fn,flim = (2000, 8000))

            #H
            H_shrimp = util.entropy(filtered)

            #Ht
            #Ht_shrimp = features.temporal_entropy(filtered, Nt = 256)

            #Hf
            Hf_shrimp, Ht_per_bin = features.frequency_entropy(Sxx)

            #M, amplitude index
            M_shrimp = features.temporal_median(filtered, Nt = 256)

            #append results to lists created earlier
            shrimp_ACI.append(ACI_shrimp) 
            shrimp_ADI.append(ADI_shrimp) 
            shrimp_BI.append(BI_shrimp)
            shrimp_H.append(H_shrimp) 
            #shrimp_Ht.append(Ht_shrimp) 
            shrimp_Hf.append(Hf_shrimp) 
            shrimp_M.append(M_shrimp) 
            
            
            ####### full band
            #filter track
            filtered = sound.select_bandwidth(x = sliced_file, fs = fs, fcut = [50, 8000], forder = 5,
                                fname = 'butter', ftype = 'bandpass')

            #compute spectrogram for: ACI, ADI, Hf
            Sxx, tn, fn, ext = sound.spectrogram (filtered, fs, nperseg = 256)  

            #ACI
            _, _ , ACI_full = features.acoustic_complexity_index(Sxx)

            #ADI, uses bins 10 bins, each 1/10th of the full freq range
            ADI_full = features.acoustic_diversity_index(Sxx, fn, fmin = 50, fmax = 8000, bin_step = 190, dB_threshold = -50)

            #BI
            BI_full = features.bioacoustics_index(Sxx, fn, flim = (50, 8000))

            #H
            H_full = util.entropy(filtered)

            #Ht
            #Ht_full = features.temporal_entropy(filtered, Nt = 256)

            #Hf
            Hf_full, Ht_per_bin = features.frequency_entropy(Sxx)

            #M, amplitude index
            M_full = features.temporal_median(filtered, Nt = 256)

            #append results to lists created earlier
            full_ACI.append(ACI_full) 
            full_ADI.append(ADI_full) 
            full_BI.append(BI_full)
            full_H.append(H_full) 
            #full_Ht.append(Ht_full) 
            full_Hf.append(Hf_full)
            full_M.append(M_full) 

            

            #compute NDSI 
            #running without reapplying a filter massively slows down the script, unsure why? 
            filtered = sound.select_bandwidth(x = sliced_file, fs = fs, fcut = [50, 20000], forder = 5,
                                    fname = 'butter', ftype = 'bandpass')
            Sxx, tn, fn, ext = sound.spectrogram (sliced_file, fs, nperseg = 256)
            NDSI, ratioBA, antroPh, bioPh  = features.soundscape_index(Sxx, fn, flim_bioPh = (2000, 8000), flim_antroPh = (50, 2000))
            NDSI_fullband.append(NDSI) 

            #store averages for 1 min
              #could maybe put these lists into a master list and execute a commond over each value of this to make code neater
            #mean_slices_time = zero_time1 #+ timedelta(milliseconds=960) #0.96s was subtracted from zero_time ################################################### do i need this line?

            if count == 61: #0.96s goes into 1min 62 times, adjust depending on length of time wanted
                #take the average of the slices for the length of time period chosen
                fish_ACI_1min_mean.append(mean(fish_ACI_temp)) 
                fish_ADI_1min_mean.append(mean(fish_ADI_temp)) 
                fish_BI_1min_mean.append(mean(fish_BI_temp)) 
                fish_H_1min_mean.append(mean(fish_H_temp)) 
                #fish_Ht_1min_mean.append(mean(fish_Ht_temp)) 
                fish_Hf_1min_mean.append(mean(fish_Hf_temp)) 
                fish_M_1min_mean.append(mean(fish_M_temp)) 

                shrimp_ACI_1min_mean.append(mean(shrimp_ACI_temp)) 
                shrimp_ADI_1min_mean.append(mean(shrimp_ADI_temp)) 
                shrimp_BI_1min_mean.append(mean(shrimp_BI_temp)) 
                shrimp_H_1min_mean.append(mean(shrimp_H_temp)) 
                #shrimp_Ht_1min_mean.append(mean(shrimp_Ht_temp)) 
                shrimp_Hf_1min_mean.append(mean(shrimp_Hf_temp)) 
                shrimp_M_1min_mean.append(mean(shrimp_M_temp)) 
                
                full_ACI_1min_mean.append(mean(full_ACI_temp)) 
                full_ADI_1min_mean.append(mean(full_ADI_temp)) 
                full_BI_1min_mean.append(mean(full_BI_temp)) 
                full_H_1min_mean.append(mean(full_H_temp)) 
                #full_Ht_1min_mean.append(mean(full_Ht_temp)) 
                full_Hf_1min_mean.append(mean(full_Hf_temp)) 
                full_M_1min_mean.append(mean(full_M_temp)) 

                NDSI_1min_mean.append(mean(NDSI_temp)) 
                
                #standard dev
                fish_ACI_1min_std.append(stdev(fish_ACI_temp)) 
                fish_ADI_1min_std.append(stdev(fish_ADI_temp)) 
                fish_BI_1min_std.append(stdev(fish_BI_temp)) 
                fish_H_1min_std.append(stdev(fish_H_temp)) 
                #fish_Ht_1min_std.append(stdev(fish_Ht_temp)) 
                fish_Hf_1min_std.append(stdev(fish_Hf_temp)) 
                fish_M_1min_std.append(stdev(fish_M_temp)) 

                shrimp_ACI_1min_std.append(stdev(shrimp_ACI_temp)) 
                shrimp_ADI_1min_std.append(stdev(shrimp_ADI_temp)) 
                shrimp_BI_1min_std.append(stdev(shrimp_BI_temp)) 
                shrimp_H_1min_std.append(stdev(shrimp_H_temp)) 
                #shrimp_Ht_1min_std.append(stdev(shrimp_Ht_temp)) 
                shrimp_Hf_1min_std.append(stdev(shrimp_Hf_temp)) 
                shrimp_M_1min_std.append(stdev(shrimp_M_temp)) 
                
                full_ACI_1min_std.append(stdev(full_ACI_temp)) 
                full_ADI_1min_std.append(stdev(full_ADI_temp)) 
                full_BI_1min_std.append(stdev(full_BI_temp)) 
                full_H_1min_std.append(stdev(full_H_temp)) 
                #full_Ht_1min_std.append(stdev(full_Ht_temp)) 
                full_Hf_1min_std.append(stdev(full_Hf_temp)) 
                full_M_1min_std.append(stdev(full_M_temp)) 

                #NDSI_1min_std.append(stdev(NDSI_temp)) #this is causing an error for no reason, S/O says its a source code bug
                NDSI_1min_std.append(np.std(NDSI_temp))

                #reset lists to blank and counter to zero
                fish_ACI_temp = []
                fish_ADI_temp = []
                fish_BI_temp = []
                fish_H_temp = []
                #fish_Ht_temp = []
                fish_Hf_temp = []
                fish_M_temp = []

                shrimp_ACI_temp = []
                shrimp_ADI_temp = []
                shrimp_BI_temp = []
                shrimp_H_temp = []
                #shrimp_Ht_temp = [] 
                shrimp_Hf_temp = []
                shrimp_M_temp = []
                
                full_ACI_temp = []
                full_ADI_temp = []
                full_BI_temp = []
                full_H_temp = []
                #full_Ht_temp = []
                full_Hf_temp = []
                full_M_temp = []

                NDSI_temp = []

                count = 0
                mean_slice_time = mean_slice_time + timedelta(minutes=1) #for 1min average  
                string_time1 = mean_slice_time.strftime('%H.%M.%S')
                mean_track_name = filename[:-4] + 'T' + string_time1 + '.wav'
                #mean_slice_names.append(mean_track_name)   ######### changed for using presliced files
                mean_slice_names.append(filename) 

            else:
                count = count + 1

                fish_ACI_temp.append(ACI_fish) 
                fish_ADI_temp.append(ADI_fish) 
                fish_BI_temp.append(BI_fish)
                fish_H_temp.append(H_fish) 
                #fish_Ht_temp.append(Ht_fish) 
                fish_Hf_temp.append(Hf_fish)
                fish_M_temp.append(M_fish) 

                shrimp_ACI_temp.append(ACI_shrimp) 
                shrimp_ADI_temp.append(ADI_shrimp) 
                shrimp_BI_temp.append(BI_shrimp)
                shrimp_H_temp.append(H_shrimp) 
                #shrimp_Ht_temp.append(Ht_shrimp) 
                shrimp_Hf_temp.append(Hf_shrimp) 
                shrimp_M_temp.append(M_shrimp)
                
                full_ACI_temp.append(ACI_full) 
                full_ADI_temp.append(ADI_full) 
                full_BI_temp.append(BI_full)
                full_H_temp.append(H_full) 
                #full_Ht_temp.append(Ht_full) 
                full_Hf_temp.append(Hf_full)
                full_M_temp.append(M_full) 

                NDSI_temp.append(NDSI)    

#calculate indices for all files in 'audio_directory', which end in .wav

#initiate empty lists for 0.96s results, these will later be stored in a df
filenames = []
slice_names = []

fish_ACI = []
fish_ADI = []
fish_BI = []
fish_H = []
fish_Ht = []
fish_Hf = []
fish_M = []
fish_H1 = []

shrimp_ACI = []
shrimp_ADI = []
shrimp_BI = []
shrimp_H = []
shrimp_Ht = []
shrimp_Hf = []
shrimp_M = []
shrimp_H1 = []

full_ACI = []
full_ADI = []
full_BI = []
full_H = []
full_Ht = []
full_Hf = []
full_M = []
full_H1 = []

NDSI_fullband = [] #NDSI compares sound between fish and shrimp band, hence stands alone


#initiate empty lists which will store the index results averaged over a longer timeframe (e.g 1min)
fish_ACI_1min_mean = []
fish_ADI_1min_mean = []
fish_BI_1min_mean = []
fish_H_1min_mean = []
fish_Ht_1min_mean = []
fish_Hf_1min_mean = []
fish_M_1min_mean = []

shrimp_ACI_1min_mean = []
shrimp_ADI_1min_mean = []
shrimp_BI_1min_mean = []
shrimp_H_1min_mean = []
shrimp_Ht_1min_mean = [] 
shrimp_Hf_1min_mean = []
shrimp_M_1min_mean = []

full_ACI_1min_mean = []
full_ADI_1min_mean = []
full_BI_1min_mean = []
full_H_1min_mean = []
full_Ht_1min_mean = []
full_Hf_1min_mean = []
full_M_1min_mean = []

NDSI_1min_mean = []

fish_ACI_1min_std = []
fish_ADI_1min_std = []
fish_BI_1min_std = []
fish_H_1min_std = []
fish_Ht_1min_std = []
fish_Hf_1min_std = []
fish_M_1min_std = []

shrimp_ACI_1min_std = []
shrimp_ADI_1min_std = []
shrimp_BI_1min_std = []
shrimp_H_1min_std = []
shrimp_Ht_1min_std = [] 
shrimp_Hf_1min_std = []
shrimp_M_1min_std = []

full_ACI_1min_std = []
full_ADI_1min_std = []
full_BI_1min_std = []
full_H_1min_std = []
full_Ht_1min_std = []
full_Hf_1min_std = []
full_M_1min_std = []

NDSI_1min_std = []

mean_slice_names = []

#execute the index_loops function using the lists from above 
index_loops(audio_dir, filenames, slice_names, NDSI_fullband,
                #0.96s values
                fish_ACI, fish_ADI, fish_BI, fish_H, fish_Ht, fish_Hf, fish_M,
                shrimp_ACI, shrimp_ADI, shrimp_BI, shrimp_H, shrimp_Ht, shrimp_Hf, shrimp_M,
                full_ACI, full_ADI, full_BI, full_H, full_Ht, full_Hf, full_M,
                #1min averaged values
                fish_ACI_1min_mean, fish_ADI_1min_mean, fish_BI_1min_mean, fish_H_1min_mean,
                fish_Ht_1min_mean, fish_Hf_1min_mean, fish_M_1min_mean,
                shrimp_ACI_1min_mean, shrimp_ADI_1min_mean, shrimp_BI_1min_mean, shrimp_H_1min_mean,
                shrimp_Ht_1min_mean, shrimp_Hf_1min_mean, shrimp_M_1min_mean, 
                full_ACI_1min_mean, full_ADI_1min_mean, full_BI_1min_mean, full_H_1min_mean,
                full_Ht_1min_mean, full_Hf_1min_mean, full_M_1min_mean,
                NDSI_1min_mean,
                fish_ACI_1min_std, fish_ADI_1min_std, fish_BI_1min_std, fish_H_1min_std,
                fish_Ht_1min_std, fish_Hf_1min_std, fish_M_1min_std,
                shrimp_ACI_1min_std, shrimp_ADI_1min_std, shrimp_BI_1min_std, shrimp_H_1min_std,
                shrimp_Ht_1min_std, shrimp_Hf_1min_std, shrimp_M_1min_std, 
                full_ACI_1min_std, full_ADI_1min_std, full_BI_1min_std, full_H_1min_std,
                full_Ht_1min_std, full_Hf_1min_std, full_M_1min_std,
                NDSI_1min_std,
                mean_slice_names)

#save results from 0.96s slice lists to df
# Ht and AR were removed as they highly correlate with Hf (see: Williams et al (2022). Ecological Indicators)
index_results = pd.DataFrame(
    {'slice': slice_names,
     
     'fish_ACI': fish_ACI,
     'fish_ADI': fish_ADI,
     'fish_H': fish_H,
     #'fish_Ht': fish_Ht,
     'fish_Hf': fish_Hf,
     'fish_M': fish_M,
     'fish_BI': fish_BI,
     #'fish_AR': fish_AR,
     
     'shrimp_ACI': shrimp_ACI,
     'shrimp_ADI': shrimp_ADI,
     'shrimp_BI': shrimp_BI,
     'shrimp_H': shrimp_H,
     #'shrimp_Ht': shrimp_Ht,
     'shrimp_Hf': shrimp_Hf,
     'shrimp_M': shrimp_M,
     #'shrimp_AR': shrimp_AR,
     
     'full_ACI': full_ACI,
     'full_ADI': full_ADI,
     'full_H': full_H,
     #'full_Ht': full_Ht,
     'full_Hf': full_Hf,
     'full_M': full_M,
     'full_BI': full_BI,
     #'full_AR': full_AR,
     
     'NDSI': NDSI_fullband,
    })

#save results from 1min mean lists to df
mean_index_results = pd.DataFrame(
    {'minute': mean_slice_names, #this needs fixing to record time like the 1s stuff does
     
     'fish_ACI': fish_ACI_1min_mean,
     'fish_ACI_std': fish_ACI_1min_std,
     'fish_ADI': fish_ADI_1min_mean,
     'fish_ADI_std': fish_ADI_1min_std,
     'fish_H': fish_H_1min_mean,
     'fish_H_std': fish_H_1min_std,
     #'fish_Ht': fish_Ht_1min_mean,
     #'fish_Ht_std': fish_Ht_1min_std,
     'fish_Hf': fish_Hf_1min_mean,
     'fish_Hf_std': fish_Hf_1min_std,
     'fish_M': fish_M_1min_mean,
     'fish_M_std': fish_M_1min_std,
     'fish_BI': fish_BI_1min_mean,
     'fish_BI_std': fish_BI_1min_std,
     #'fish_AR': fish_AR,
     #'fish_AR_std': fish_AR,
     
     'shrimp_ACI': shrimp_ACI_1min_mean,
     'shrimp_ACI_std': shrimp_ACI_1min_std,
     'shrimp_ADI': shrimp_ADI_1min_mean,
     'shrimp_ADI_std': shrimp_ADI_1min_std,
     'shrimp_BI': shrimp_BI_1min_mean,
     'shrimp_BI_std': shrimp_BI_1min_std,
     'shrimp_H': shrimp_H_1min_mean,
     'shrimp_H_std': shrimp_H_1min_std,
     #'shrimp_Ht': shrimp_Ht_1min_mean,
     #'shrimp_Ht_std': shrimp_Ht_1min_std,
     'shrimp_Hf': shrimp_Hf_1min_mean,
     'shrimp_Hf_std': shrimp_Hf_1min_std,
     'shrimp_M': shrimp_M_1min_mean,
     'shrimp_M_std': shrimp_M_1min_std,
     #'shrimp_AR': shrimp_AR,
     #'shrimp_A_stdR': shrimp_AR,
     
     'full_ACI': full_ACI_1min_mean,
     'full_ACI_std': full_ACI_1min_std,
     'full_ADI': full_ADI_1min_mean,
     'full_ADI_std': full_ADI_1min_std,
     'full_H': full_H_1min_mean,
     'full_H_std': full_H_1min_std,
     #'full_Ht': fish_Ht_1min_mean,
     #'full_Ht_std': fish_Ht_1min_std
     'full_Hf': full_Hf_1min_mean,
     'full_Hf_std': full_Hf_1min_std,
     'full_M': full_M_1min_mean,
     'full_M_std': full_M_1min_std,
     'full_BI': full_BI_1min_mean,
     'full_BI_std': full_BI_1min_std,
     #'fish_AR': fish_AR,
     #'fish_AR_std': fish_AR,
     
     'NDSI': NDSI_1min_mean,
     'NDSI_std': NDSI_1min_std,     
    })



#view results dataframe for 1min time segments
mean_index_results

#save a timestamped csv with 1min results
now = datetime.now()
time_now = now.strftime("%H.%M.%S")
mean_index_results.to_csv(results_directory + '/Kimbe_indices' + '.csv')
