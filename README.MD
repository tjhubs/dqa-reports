![kemri_logo](doc/assets/img/kemri.png?raw=true)

***

# DATA QUALITY ASSURANCE:

***

![cin_logo](doc/assets/img/cin.png?raw=true)

## Clinical Information Network 

***

#### Preface

> This is an R project written to perform concordance analysis as the objective step of the DQA process for active data repositories maintained by the Health Systems Research Group of KEMRI Wellcome Trust.

> This heavily depends on a package __[```RedcapData```](http://github.com/bonmac/RedcapDqa)__ (developed by the team) which abstracts much of the data-allied and computational functionality required to perform the audit and __[```ReporteRs```](http://github.com/davidgohel/ReporteRs)__ (credit to David Gohel) which is used to generate and format the resultant word document.

### The DQA process

> For a specific DQA period, data is selected based on a strict set of predefined criteria and then subsequently sampled to obtain the desired sample size usually 5 - 10 records. The record identifiers of these records and the corresponding IP numbers abstracting the physical files are then noted down and send to the data clerks at the sites for prompt retrieval.

> During the site visits, the attending data manager picks the files and abstracts the data carefully into a clone repository with an exact underlying data schema (metadata) as the live data repository being audited. This is done at the sites after which the audit repository is synced with its master located at the data center.

> At the data center, using the DQA audit database as the gold standard, a concordance analysis is run as a measure of consistency across the two repositories. The residual analysis is documented in a word report that serves as the output of the audit process.

***

### Technical Details

#### Language

* R (as a DSL)
* Shell Scripting

#### Run Time Environment

* R (as an runtime engine)
* REDCap (CDBMS)

#### Development Environment

* RStudio
* Git

***

#### Team

* Lead
    * Boniface Makone
* Others
    * Michael Bitok
    * Wycliffe Nyachiro
    * George Mbevi
    
    
    
***

_[KEMRI Wellcome Trust](http://www.kemri-wellcome.org) © 2015 | All Rights Reserved_

***
