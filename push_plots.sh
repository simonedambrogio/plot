#!/bin/bash

cd '/Users/simonedambrogio/Dropbox/Ongoing/Uncertainty/Task/Monkey/PsychoPy/plot'
echo 'Running R codes... '
Rscript code.R
git pull
git add "Zack"
git add "Zap"
git add "Zeno"
git commit -m "update"
git push

