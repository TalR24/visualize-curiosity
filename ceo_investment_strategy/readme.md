This post was made in two parts on the Visualize Curiosity website.

Notes for Part II:

All historical stock performance data was pulled from yahoo.finance.com. Performance was pulled as monthly data, selecting for closing price. Dates for CEOs were found using simple Google searches, and so may be slightly inaccurate. Start and end dates pulled from official company website where available and found.

Data was then imported in STATA, where I cleaned it and merged in S&P 500 Index data. Values were then indexed to oldest closing date and using a simple growth formula [(closing price at date N)/(closing price at date 1)].

The datasets were then exported and brought into R. All visuals were made in R, using the following packages: readr, ggplot2, dplyr, RColorBrewer, ggthemes, tidyverse, stringr. I styled my charts after one of my favorite websites, FiveThirtyEight, using the ggtheme of the same name.

You may have also noticed that the line colors for each company are that company’s logo colors! Credit to http://www.codeofcolors.com/brand-colors.html for providing the hex color codes for each company. Also credit to http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf and colorbrewer2.org for providing additional color schemes and R color information.

If you want me to take a look at historical performance of other stocks, or simply have questions or constructive feedback, feel free to email me at troded24@gmail.com, submit an inquiry on this website, or leave a comment on this post! Thanks for reading.


Notes for Part I:
All historical stock performance data was pulled from yahoo.finance.com. Performance was pulled as monthly data, selecting for closing price. Dates for CEOs were found using simple Google searches, and so may be slightly inaccurate. Start and end dates pulled from official company website where available and found.

Data was then imported in STATA, where I cleaned it and merged in S&P 500 Index data. Values were then indexed to oldest closing date and using a simple growth formula [(closing price at date N)/(closing price at date 1)].

The datasets were then exported and brought into R. All visuals were made in R, using the following packages: readr, ggplot2, dplyr, RColorBrewer, ggthemes, tidyverse, stringr. I styled my charts after one of my favorite websites, FiveThirtyEight, using the ggtheme of the same name.

You may have also noticed that the line colors for each company are that company’s logo colors! Credit to http://www.codeofcolors.com/brand-colors.html for providing the hex color codes for each company. Also credit to http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf and colorbrewer2.org for providing additional color schemes and R color information.

Additional parts featuring other sectors will be follow soon! If there are any companies you'd like me to take a look at, leave a comment, shoot me an email, or fill out the inquiry form on this website.
