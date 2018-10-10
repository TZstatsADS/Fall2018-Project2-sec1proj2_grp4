# Project 2: Shiny App Development Version 2.0

### [Project Description](doc/project2_desc.md)

![screenshot](doc/Hospital.jpg)

## Find Your Hospital Today
Term: Fall 2018

+ Team #4
+ **KNOW YOUR HOSPITALS**: 
	+ Here is App link: (shiny app url)
 	+ Team members
		+ Zehan Wang
		+ Xiaojie Wei
		+ Ghada Jerfel
		+ Xiaoyi Li (Presenter)
		+ Peiqi Jin

+ **Project Summary**: 

Maintaining a good health is inevitably very important because it allows people to carry out daily life activities perfectly well. As such, in modern day times, people pay attention all the more to their personal health.
In attempt to help patients better explore the comparative cost of procedures and estimate the medical charges prior to receiving care, our group design an interactive app called "Hospital Finder" using Shiny app in R. This application will help patients find hospitals based on location, hospital type, estimated cost range and the availability of emergency services. This application also ranks all the hospitals that satisfy the criterias of certain measurments (such as safety of care of hte hospital) in order to help patients make personalized decisoins. Additionally, it visualizes interesting findings concerning the data it uses, which can be found under Key Statistics section.


+ **Data Introduction**: 
	+ Medicare Inpatient Charge Data: https://www.data.gov/health/hospital-charge-data/  
	+ Medicare Hospital Quality Data: https://data.medicare.gov/Hospital-Compare/Hospital-General-Information/xubh-q36u/data

+ **App Introduction**: The application has 3 sections:

	+ Hospital Recommendation: By selecting state, hospital type, disease type, expected cost range and emergency service, the app shows all tbe hospitals on the map and that satisfy such selections. At the bottom of this display, the app includes a  general information table about the hospitals.
	+ Key Statistic: According to different type of disease this part will show average total cost in each states. This is general statistic information about hospital.
	+ FAQ: It shows all the information about our data and all the instructions about our app. Please see FAQ if you have any questions.
	
	![screenshot](doc/abc.png)


+ **Contribution Statement**: 
All team members are actively involved in all stages of this project and help design the app. All team members combined and modified codes together.
  + Zehan Wang: Designed the ui and server part of "Hospital recommendation" as well as the hospital map
  + Xiaojie Wei: Designed the ui and server part of "Key Statistics"
  + Ghada Jerfel: Designed the ui and server part of "Key Statistics"
  + Xiaoyi li: Designed the ui and server part of "Hospital Recommendation"
  + Perqi Jin: Desigedn the ui and server part of "Hospital Recommendation"







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

