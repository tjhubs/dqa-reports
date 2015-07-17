# DATA QUALITY ASSURANCE:

***

## Clinical Information Network 

***

#### Preface

> This project is a syndicated R application written to perform concordance analysis as the objective step of the DQA process for projects within the Health Systems Research Group of KEMRI Wellcome Trust.

> This heavily depends on a Package __[```RedcapData```](http://github.com/bonmac/RedcapDqa)__ which abstracts much of the computational functionality and __[```ReporteRs```](http://github.com/davidgohel/ReporteRs)__ which is used to generate and format the resultant word document.

### The DQA process

> For a specific DQA period, data is selected based on a strict set of predefined criteria and then subsequently sampled to obtain the desired sample size usually 5 - 10 records. The record identifiers of these records and the corresponding IP numbers abstracting the physical files are then noted down and send to the data clerks at the sites for prompt retrieval.

> During the site visits, the attending data manager picks the files and abstracts the data carefully into a clone repository with an exact underlying data schema (metadata) as the live data repository being audited. This is done at the sites after which the repository is synced with the master at the data center.

> Using the DQA audit database as the glod standard, a concordance analysis is run as a measure of consistency across the two repositories. This is documented in a word report that serves as the output of the process.

***

### Technical Details

#### Language

* R
* Shell Scripting

#### Run Time Environment

* R (as an engine)
* REDCap (CDBMS)

#### Development Environment

* RStudio
* Git

***

#### Team

* Lead
    * Boniface Makone
* Others
    * Michael Botok
    * Wycliffe Nyachiro
    * George Mbevi
    
    
    
***

_[KEMRI Wellcome Trust](http://www.kemri-wellcome.org) © 2014 | All Rights Reserved_

***
