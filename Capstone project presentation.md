Capstone project presentation
========================================================
author: Paul van der Kooy
date:  17 December 2016
autosize: true


========================================================
# **The challenge**
The objective of the project is to develop a text prediction application like those used on mobile telephones and app's like WhatsApp.

- Exploratory data analysis
    Use different sources (Twitter, News & Blogs) to find common text sequences  
    The approach described in the following article: <http://rpubs.com/paulkooy/229890>  
    
- Build a text prediction model
    A model based on N-grams was developed an tested to find optimal setting for performance  
    Next words are predicted using tables of frequently used word combinations (N-Grams). N is the number of words in a sequence. Preference is given to the highest N-gram with the highest occurrence frequency in a representative set of sample texts (the corpus)  
    
- Build a text prediction application on the Web
    The Web based application implements the model into a public available user interface, which suggest the next word of entered text and maintains a prediction score  
    Use the following link to start the end product: <https://paulkooy.shinyapps.io/capstoneNLP/>
    
Code for this work can be found on GitHub: <https://github.com/paulkooy/Data-science-capstone>


========================================================
# **Design considerations**

## **Optimizations**
Further improvements considered but not implemented because the results were not significant or due to time pressure to deliver the product in the allocated time.  
- Addition of common phrases or idioms, expressions and sayings or one liners
- Organize the N-grams into an associative network or Markov chain for faster retrieval  
Not implemented because the N-grams list can be significantly reduced without impacting the prediction score to much. Hence the performance benefit is minimal.  
- Implement the Katz's back-off model
- Stemming (process of reducing inflected or sometimes derived words to their word stem)  
Was not implemented as standard simple procedures often produce strange incorrect stems, while a good algorithm is complex  
- Filtering the model input (N-grams) on spelling, via a spell checker  
- Use fuzzy logic like <span style="color:red">`agrep`</span> or <span style="color:red">`Levenshtein distance`</span> to overcome small differences in spelling caused by typo's or grammar
- Add N-grams of incorrectly predicted words to the dataset (self improving model)  
This method will allow the model to adjust to personal language and writing style

***
## .  
## .  
## **Drawbacks**
Most drawbacks of the N-grams method are related to its statistical background and missing context of the provided text.  
- The N-grams model is purely statistical driven, which can impact the results when complex, specialistic jargon or academic language, or slang is used that was not part of the text samples to create the N-grams
- In phrases that have about an equal change of a commonly use next word the model prediction can be arbitrary. E.g. *<span style="color:blue">`"lot of food"`</span>*  versus *<span style="color:blue">`"lot of different"`</span>*
- The model shows clear examples where frequency logic overrules an obvious context. For example: *<span style="color:blue">`"Often I go running, because I love ???"`</span>*. Here the N-grams logic will predict *<span style="color:blue">`"love you"`</span>* before *<span style="color:blue">`"love it"`</span>*.  
Longer N-grams will compensate partly for missing context, but cannot replace it.

========================================================
# **Prediction model test results**
<img src="Capstone project presentation-figure/unnamed-chunk-1-1.png" title="plot of chunk unnamed-chunk-1" alt="plot of chunk unnamed-chunk-1" width="1540" />

predictScore    = The percentage correctly predicted words  
maxNgrams       = The highest level od N-grams used for the test  
nGramsUsed      = The percentage of the N-grams model data set used for this test  

## **Conclusion:**  
- Using upto 5-grams with a relative small dataset between 10 and 50% of the data results in a relative good prediction score against acceptable performance. The final model chosen was with 50% of the dataset and using upto 5-Grams and resulting of an average prediction success of ~17%.
- Size of the model dataset had no effect on the memory usage of the model. It remained steady independent of the amount of model N-grams I used (even after removing the initial dataset and garbage collection)
- The execution time proportional to the volume of model N-grams


========================================================
# **The application** <small>(end result)</small>

The App usage is very simple. Just type any text in the side panel. The complete text plus predicted words are presented in the main panel.  
![Screen Shot](screenshot.png)
Further improvement opportunities:
* Make the predicted words selectable (like in WhatsApp)  
* Avoid equal predictions from different N-grams with same result  
* Improve the prediction rate (see slide 3: Design considerations)