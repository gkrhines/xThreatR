# xThreatR
An implementation of Karun Singh's Expected Threat with R. Data in the SPADL form is required.

The RScripts included in this repository will allow you to implement the Expected Threat (xT) model laid out in Karun Singh's blog post here: https://karun.in/blog/expected-threat.html

The inspiration for the main script came from the following repository: https://github.com/ML-KULeuven/socceraction/tree/master/socceraction. Also, the included data was processed from Statsbomb's open data repository 

## How to Use
### xThreat.R
To use this RScript effectively, simply take some data from the Statsbomb Data file and input it into the xThreat.R script. The xThreat Applied.R file showcases an example of this and can be used to be the basis of a project using the xT model.

### Statsbomb Getter MkII.R
To get this RScript to work, all you have to first download event data from Statsbomb's open data repo: https://github.com/statsbomb/StatsBombR. Then change the path on line 361 to contain those files. Everything should go swimmingly after that.

## SPADL Format

https://arxiv.org/pdf/1802.07127.pdf
