# fastverse 0.1.8
In version 0.1.7 I had implemented all CRAN comments, except for placing return values on `fastverse_extend()`, `fastverse_detach()`, and `fastverse_reset()`, which do not return anything as clearly evident from the function title and description. In mentioning this to CRAN, I have also pointed out that the `tidyverse` package has 6 functions similar in nature, non of which have a return value statement. I was refused with the words 'no need to copy bad practice'. Wondering why CRAN is not consequent enough to purge this ostensible bad practice off CRAN, I want to say I agree with Hadley Wickham: functions who are not meant to return anything should not be required to have a return value, in contrast to functions which are meant to return something. This makes for clear and parsimonious documentation. So the CRAN version now gets return values - with some necessary critique - but I will spare you this in the GitHub version. My apologies to all CRAN version users. 

# fastverse 0.1.7
Implementing various CRAN comments on 0.1.6 (small things), and setting up synchronous development of CRAN and GitHub version through a 'development' branch which can be merged into both. Pull requests should be sent to the 'development' branch. 

# fastverse 0.1.6
CRAN rejected 0.1.5, which made sure *matrixStats* handles attributes consistently whenever the *fastverse* is attached, because CRAN packages are not allowed to modify the namespace of other packages. Thus 0.1.6 comes in two versions: A CRAN version which takes *matrixStats* as it is, and a GitHub version which is the original 0.1.5. User who value a consistent *matrixStats* that preserves dimension names in all functions are recommended the GitHub version. 

# fastverse 0.1.5
First *fastverse* CRAN submission on 7th August 2021. Development started in Spring 2021. 
