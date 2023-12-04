# privilege_walk

This repo contains code to analyse results of a virtual privilege walk. 

## privilege.Rmd 
This is the main Rmd to execute to render the privilege walk analyses into 
slides. 
Make sure to set use_simulated_data on line 13 to be FALSE if you want to use 
the real data and TRUE if you want to use simulated data (see also 
simulate_data.R below).

## source_code.R 
This contains the code which reads in the data. You will need to change the link 
on line 25 to point to your own google sheet. 
The code is set to read answers from a google form like this one: 
https://bit.ly/privilege-template
So you will need to point to the google sheet where the answers to your form
are collated.

## simulate_data.R
This is a file I used to simulate data so that I could build graphs that would 
look good even with hundreds of respondents. Running it will generate a new rds
file simulated_data.rds; this is the file which is used when you run 
privilege.Rmd with use_simulated_data set to TRUE. 