#!/bin/bash
echo "export EDITOR=vim" >> ~/.bash_rc


echo "deb http://cran.wustl.edu/bin/linux/ubuntu precise/" >> /etc/apt/sources.list
apt-get update
apt-get install python r-base gdebi-core libapparmor1 python-pip liblas-bin screen vim enchant python-numpy python-scipy python-liblas liblas-bin
pip install cython scikit-learn pandas mechanize PyEnchant nltk
wget http://download2.rstudio.org/rstudio-server-0.97.237-amd64.deb
sudo gdebi rstudio-server-0.97.237-amd64.deb
echo 'install.packages(c("plyr", "MASS", "randomForest", "gbm", "ggplot2")' | R --vanilla
echo 'install.packages(c("caret"), dependencies=c("Suggests", "Depends"))' | R --vanilla
