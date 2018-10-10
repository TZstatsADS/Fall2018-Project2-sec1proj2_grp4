# Project 2: Shiny App Development Version 2.0

### [Project Description](doc/project2_desc.md)

![screenshot](doc/Hospital.jpg)

## Find Your Hospital Today (shiny app url)
Term: Fall 2018

+ Team #4
+ **KNOW YOUR HOSPITALS**: 
+ Team members
	+ team member 1 Zehan Wang
	+ team member 2 Xiaojie Wei
	+ team member 3 Ghada Jerfel
	+ team member 4 Xiaoyi Li
	+ team member 5 Peiqi Jin

+ **Project summary**: Good health is very important because a person of good health can put through a large amount of work in a short time. So people put more attention on personal health in today's society.In attempt to help patients better explore the comparative price of procedures and estimate their medical service prior to receiving care, Our group design an interactive finding hospital App using Shiny app in R. This APP will help patients to do some filter of hospital such as location,hospital type,expection cost range and emergency service. Also we rank all the hospitals that satisfy our choosing criticals in order to help patients make dicisons. We also visualize the average total cost of state and average total cost of disease


+ **Data introduction**: 
	+ Medicare Hospital Quality Data: https://data.medicare.gov/Hospital-Compare/Hospital-General-Information/xubh-q36u/data
	+ Medicare Inpatient Charge Data: https://www.data.gov/health/hospital-charge-data/  

+ **App introduction**:
The application has 3 sections:

+ Hospital Recommendation: Selecting State,hospital type,disease type,expection cost range and emergency service then all hospitals that satisfies those criterions will show on the map. Then at the bottom of this part it will shows the hospital general information table 
+ Key Statistic : According to different type of disease this part will show average total cost in each states. This is general statistic information about hospital.
+ FAQ: It shows all the information about our data and all the instructions about our app. Please see FAQ if you have any questions.

+ **Contribution statement**: 
All team members are actively involved in all stages of this project and help design the app. All team members combined and modified codes together.
  + Zehan Wang: Mainly responsible to create map for hospital and design the ui and server part of "Hospital recommendation"
  + Xiaojie Wei: Designed the ui and server part of "key statistic"
  + Ghada Jerfel: Designed the ui and server part of "key statistic"
  + Xiaoyi li: Designed the ui and server part of "Hospital recommendation"
  + Perqi Jin: Desigedn the ui and server part of "Hospital recommendation"







Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── app/
├── lib/
├── data/
├── doc/
└── output/
```

Please see each subfolder for a README file.

