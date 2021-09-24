# Oto_abR
## _Information_

This repository has been uploaded for the development and presentation of the platform's UI. The deployed platform can be found at: https://autoabr.shinyapps.io/_abr_shiny_final/

The "Ωto_abR" app was created in order to visualize and/or analyze an Acoustic Brainstem Response (ABR) which was extracted, using the Interacoustics Eclipse Platform (module EP15). Additional information about the Eclipse platform can be found at https://www.interacoustics.com/abr/eclipse and https://www.manualslib.com/manual/1365514/Interacoustics-Eclipse.html .

The first word of our app’s name is a wordplay between the english word “auto” (automation) and the greek word for ear, “ους” or “ωτός”, which is phonetically pronounced similar to auto. Click-Evoked ABR testing proves to be invaluable, as it provides an objective representation of auditory function, an estimate of hearing thresholds and the ability to pinpoint a potential issue in the auditory neural pathway.


## _How to use the app!_

* Step 1 - Upload a Patient's test file (.xml)  
On the "_Home_" Tab click on the "_Browse_" button and select the ABR (.xml file), you would like to analyze. **Ωto_abr** will automatically present the patient's ABR test and potential Jewett Peaks/Valleys.

* Step 2 - Change automatically detected Jewett Peaks/Valleys  
After the initial plots are displayed, **Ωto_abr** gives you the option to change the detected Jewett peaks and valleys. To do this, you have to click on the checkbox under the respective field and choose which peaks and valleys should be changed on the waveform.Once the peaks or valleys are selected, a slider control is presented to the user, that corresponds to each peak/valley. By changing the slider values, the respective peak or valley is dynamically changed on the ABR waveform. You also have the option to begin a new patient analysis, by pressing the “New Patient Analysis” button.

* Step 3 - ABR Analysis Tab  
This page displays visual representations of the ABR waveform analysis (Figure 8). There are four analysis plots in total and there is a subtab for each one. The mentioned plots are shown in Figure 9 and are the following:
    1. ABR Area Analysis
    2. Latency/Intensity Function  - Peak I
    3. Latency/Intensity Function  - Peak III
    4. Latency/Intensity Function  - Peak V
    
In this page, users are given the option to save the plots in a .pdf file and the amplitude, latency and area analysis to .csv files. Finally, the “New Patient Analysis” button is also present. 


## _Creators_

Aristotelis Ballas | - | Panagiotis Katrakazas
------------- | ------------- | -------------
School of Electrical and Computer Engineering| | Biomedical Engineering Laboratory
National Technical University of Athens|        | National Technical University of Athens
Athens, Greece | | Athens, Greece
telis.ballas@gmail.com | | pkatrakazas@biomed.ntua.gr

