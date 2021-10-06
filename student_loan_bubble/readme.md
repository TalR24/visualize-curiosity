This post was split into two parts on the Visualize Curiosity website.

Notes for Part II:

Voter registration data was collected from the following sources:

[Pennsylvania](https://www.dos.pa.gov/VotingElections/OtherServicesEvents/VotingElectionStatistics/Pages/Voter-Registration-Statistics-Archives.aspx)

[Florida](https://dos.myflorida.com/elections/data-statistics/voter-registration-statistics/voter-registration-reportsxlsx/)

[North Carolina](https://www.ncsbe.gov/results-data/voter-registration-data)

Charts seen in this post were made in R using the ggplot2, tidyverse, readxl, RColorBrewer, and Cairo packages.

If you have questions or constructive feedback, feel free to email me at troded24@gmail.com, submit an inquiry on this website, or leave a comment on this post! Thanks for reading, and please - make sure to vote this election.


Notes for Part I:

Note: More information and resources on student loans are available at lendedu.com. For an especially deeper dive into the pros and cons of private student loans see https://lendedu.com/blog/what-to-consider-before-taking-out-a-private-student-loan/.

Other notes on overall data: Total may not be exactly equal to 100% or the sum of their parts due to rounding. Time-series data is by federal fiscal year which ends September 30. Data for 2018 is currently available for up to Q2 which ends March 30, 2018. “Recipient” refers to the receiver of the loan, most often the student but can also be the parent of said student.

Loan School Types Notes: Balance is total outstanding principal and interest balances of federal student loans in Q2 2018. "Other" includes consolidation loans made prior to 2004 that cannot currently be linked to a specific school in the Enterprise Data Warehouse.  Includes Direct Loan, Federal Family Education Loan, and Perkins Loan borrowers in an Open loan status. Recipient counts are based at the loan level. If a recipient received loans from more than one school type, they are counted in each applicable school type. There were also two other categories in the data: foreign schools and “other”. Since foreign schools only constituted about 1% of the total loan market I decided to exclude that category. “Other” was a bit more significant, about 6% of the total loan balance, but that category simply consists of loans that cannot be traced to a specific school (due to consolidation or other factors) so I also decided to exclude it.

For more information on the differences between loan types and details on the terms of each, check out https://studentaid.ed.gov/sa/types/loans and https://studentaid.ed.gov/sa/sites/default/files/federal-loan-programs.pdf.

Data was collected from https://studentaid.ed.gov/sa/about/data-center/student/portfolio, cleaned and transformed in STATA, then visualized using ggplot2 in R.
