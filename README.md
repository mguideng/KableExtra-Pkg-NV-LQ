README
================
Maria Guideng

kableExtra-Pkg-NV-LQ
====================

Application of kableExtra, an R package
---------------------------------------

Looking for a way to generate a formatted table in R? No? Well now you can!

R is not that great for final product data visualization, but if there was a simple way to give my plain tables a little love, I figured, 'Why not?' So I asked The Google, and it brought me to [this article](https://haozhu233.github.io/kableExtra/awesome_table_in_html.html) by Hao Zhu, creator of the [kableExtra](https://CRAN.R-project.org/package=kableExtra) R package. The features to manipulate table styles seemed straightforward enough for a noob like me with no experience programming in CSS, so I decided to give it a run. The lucky table chosen for the makeover was from tabular data to calculate location quotients (LQs) in Las Vegas. The LQs quantified how concentrated a particular industry is in a region as compared to the nation. Specifically, I wanted to highlight the obvious about my table: gaming and hospitality drives the Vegas economy. It took a few iterations, but I figured out how to style everything the way I wanted (after augmentation with the `formattable` package). Check out the before and after tables below.

This is the basic HTML table in `kable`:

``` r
kable(lqtab, "html")
```

<table>
<thead>
<tr>
<th style="text-align:left;">
NAICS
</th>
<th style="text-align:left;">
Industry
</th>
<th style="text-align:left;">
US Employment
</th>
<th style="text-align:left;">
US % Share
</th>
<th style="text-align:left;">
Las Vegas Employment
</th>
<th style="text-align:left;">
Las Vegas % Share
</th>
<th style="text-align:left;">
Employment for Las Vegas Requirements
</th>
<th style="text-align:left;">
Las Vegas LQ
</th>
<th style="text-align:left;">
Las Vegas LQ Classification
</th>
<th style="text-align:left;">
Las Vegas Excess Employment (to export) or Deficit
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
Forestry
</td>
<td style="text-align:left;">
1,259,490
</td>
<td style="text-align:left;">
1.05%
</td>
<td style="text-align:left;">
513
</td>
<td style="text-align:left;">
0.06%
</td>
<td style="text-align:left;">
8,771
</td>
<td style="text-align:left;">
0.06
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
-8,258
</td>
</tr>
<tr>
<td style="text-align:left;">
21
</td>
<td style="text-align:left;">
Mining
</td>
<td style="text-align:left;">
613,389
</td>
<td style="text-align:left;">
0.51%
</td>
<td style="text-align:left;">
331
</td>
<td style="text-align:left;">
0.04%
</td>
<td style="text-align:left;">
4,272
</td>
<td style="text-align:left;">
0.08
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
-3,941
</td>
</tr>
<tr>
<td style="text-align:left;">
22
</td>
<td style="text-align:left;">
Utilities
</td>
<td style="text-align:left;">
553,007
</td>
<td style="text-align:left;">
0.46%
</td>
<td style="text-align:left;">
2,646
</td>
<td style="text-align:left;">
0.32%
</td>
<td style="text-align:left;">
3,851
</td>
<td style="text-align:left;">
0.69
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
-1,205
</td>
</tr>
<tr>
<td style="text-align:left;">
23
</td>
<td style="text-align:left;">
Construction
</td>
<td style="text-align:left;">
6,686,142
</td>
<td style="text-align:left;">
5.55%
</td>
<td style="text-align:left;">
54,652
</td>
<td style="text-align:left;">
6.51%
</td>
<td style="text-align:left;">
46,561
</td>
<td style="text-align:left;">
1.17
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
8,091
</td>
</tr>
<tr>
<td style="text-align:left;">
31-33
</td>
<td style="text-align:left;">
Manufacturing
</td>
<td style="text-align:left;">
12,296,697
</td>
<td style="text-align:left;">
10.2%
</td>
<td style="text-align:left;">
22,011
</td>
<td style="text-align:left;">
2.62%
</td>
<td style="text-align:left;">
85,632
</td>
<td style="text-align:left;">
0.26
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
-63,621
</td>
</tr>
<tr>
<td style="text-align:left;">
42
</td>
<td style="text-align:left;">
Wholesale
</td>
<td style="text-align:left;">
5,859,605
</td>
<td style="text-align:left;">
4.86%
</td>
<td style="text-align:left;">
21,544
</td>
<td style="text-align:left;">
2.57%
</td>
<td style="text-align:left;">
40,805
</td>
<td style="text-align:left;">
0.53
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
-19,261
</td>
</tr>
<tr>
<td style="text-align:left;">
44-45
</td>
<td style="text-align:left;">
Retail
</td>
<td style="text-align:left;">
15,824,396
</td>
<td style="text-align:left;">
13.13%
</td>
<td style="text-align:left;">
107,103
</td>
<td style="text-align:left;">
12.76%
</td>
<td style="text-align:left;">
110,198
</td>
<td style="text-align:left;">
0.97
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
-3,095
</td>
</tr>
<tr>
<td style="text-align:left;">
48-49
</td>
<td style="text-align:left;">
Trans/Ware
</td>
<td style="text-align:left;">
4,765,869
</td>
<td style="text-align:left;">
3.95%
</td>
<td style="text-align:left;">
38,558
</td>
<td style="text-align:left;">
4.59%
</td>
<td style="text-align:left;">
33,189
</td>
<td style="text-align:left;">
1.16
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
5,369
</td>
</tr>
<tr>
<td style="text-align:left;">
51
</td>
<td style="text-align:left;">
Information
</td>
<td style="text-align:left;">
2,796,947
</td>
<td style="text-align:left;">
2.32%
</td>
<td style="text-align:left;">
11,428
</td>
<td style="text-align:left;">
1.36%
</td>
<td style="text-align:left;">
19,477
</td>
<td style="text-align:left;">
0.59
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
-8,049
</td>
</tr>
<tr>
<td style="text-align:left;">
52
</td>
<td style="text-align:left;">
Finance
</td>
<td style="text-align:left;">
5,826,386
</td>
<td style="text-align:left;">
4.83%
</td>
<td style="text-align:left;">
25,588
</td>
<td style="text-align:left;">
3.05%
</td>
<td style="text-align:left;">
40,574
</td>
<td style="text-align:left;">
0.63
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
-14,986
</td>
</tr>
<tr>
<td style="text-align:left;">
53
</td>
<td style="text-align:left;">
RE
</td>
<td style="text-align:left;">
2,127,375
</td>
<td style="text-align:left;">
1.77%
</td>
<td style="text-align:left;">
20,450
</td>
<td style="text-align:left;">
2.44%
</td>
<td style="text-align:left;">
14,815
</td>
<td style="text-align:left;">
1.38
</td>
<td style="text-align:left;">
Export
</td>
<td style="text-align:left;">
5,635
</td>
</tr>
<tr>
<td style="text-align:left;">
54
</td>
<td style="text-align:left;">
Professional
</td>
<td style="text-align:left;">
8,840,443
</td>
<td style="text-align:left;">
7.34%
</td>
<td style="text-align:left;">
39,176
</td>
<td style="text-align:left;">
4.67%
</td>
<td style="text-align:left;">
61,563
</td>
<td style="text-align:left;">
0.64
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
-22,387
</td>
</tr>
<tr>
<td style="text-align:left;">
55
</td>
<td style="text-align:left;">
Management
</td>
<td style="text-align:left;">
2,230,131
</td>
<td style="text-align:left;">
1.85%
</td>
<td style="text-align:left;">
19,601
</td>
<td style="text-align:left;">
2.34%
</td>
<td style="text-align:left;">
15,530
</td>
<td style="text-align:left;">
1.26
</td>
<td style="text-align:left;">
Export
</td>
<td style="text-align:left;">
4,071
</td>
</tr>
<tr>
<td style="text-align:left;">
56
</td>
<td style="text-align:left;">
Admin
</td>
<td style="text-align:left;">
8,954,343
</td>
<td style="text-align:left;">
7.43%
</td>
<td style="text-align:left;">
74,822
</td>
<td style="text-align:left;">
8.92%
</td>
<td style="text-align:left;">
62,356
</td>
<td style="text-align:left;">
1.20
</td>
<td style="text-align:left;">
Export
</td>
<td style="text-align:left;">
12,466
</td>
</tr>
<tr>
<td style="text-align:left;">
61
</td>
<td style="text-align:left;">
Educational
</td>
<td style="text-align:left;">
2,766,964
</td>
<td style="text-align:left;">
2.3%
</td>
<td style="text-align:left;">
8,744
</td>
<td style="text-align:left;">
1.04%
</td>
<td style="text-align:left;">
19,269
</td>
<td style="text-align:left;">
0.45
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
-10,525
</td>
</tr>
<tr>
<td style="text-align:left;">
62
</td>
<td style="text-align:left;">
Health
</td>
<td style="text-align:left;">
18,887,301
</td>
<td style="text-align:left;">
15.67%
</td>
<td style="text-align:left;">
81,382
</td>
<td style="text-align:left;">
9.7%
</td>
<td style="text-align:left;">
131,528
</td>
<td style="text-align:left;">
0.62
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
-50,146
</td>
</tr>
<tr>
<td style="text-align:left;">
71
</td>
<td style="text-align:left;">
Arts
</td>
<td style="text-align:left;">
2,237,922
</td>
<td style="text-align:left;">
1.86%
</td>
<td style="text-align:left;">
20,540
</td>
<td style="text-align:left;">
2.45%
</td>
<td style="text-align:left;">
15,584
</td>
<td style="text-align:left;">
1.32
</td>
<td style="text-align:left;">
Export
</td>
<td style="text-align:left;">
4,956
</td>
</tr>
<tr>
<td style="text-align:left;">
72
</td>
<td style="text-align:left;">
Accommodation
</td>
<td style="text-align:left;">
13,318,703
</td>
<td style="text-align:left;">
11.05%
</td>
<td style="text-align:left;">
265,110
</td>
<td style="text-align:left;">
31.59%
</td>
<td style="text-align:left;">
92,749
</td>
<td style="text-align:left;">
2.86
</td>
<td style="text-align:left;">
Export
</td>
<td style="text-align:left;">
172,361
</td>
</tr>
<tr>
<td style="text-align:left;">
81
</td>
<td style="text-align:left;">
Other
</td>
<td style="text-align:left;">
4,387,613
</td>
<td style="text-align:left;">
3.64%
</td>
<td style="text-align:left;">
24,149
</td>
<td style="text-align:left;">
2.88%
</td>
<td style="text-align:left;">
30,555
</td>
<td style="text-align:left;">
0.79
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
-6,406
</td>
</tr>
<tr>
<td style="text-align:left;">
99
</td>
<td style="text-align:left;">
Industries not classified
</td>
<td style="text-align:left;">
271,898
</td>
<td style="text-align:left;">
0.23%
</td>
<td style="text-align:left;">
825
</td>
<td style="text-align:left;">
0.1%
</td>
<td style="text-align:left;">
1,893
</td>
<td style="text-align:left;">
0.44
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
-1,068
</td>
</tr>
<tr>
<td style="text-align:left;">
Total
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
120,504,621
</td>
<td style="text-align:left;">
100%
</td>
<td style="text-align:left;">
839,173
</td>
<td style="text-align:left;">
100%
</td>
<td style="text-align:left;">
839,172
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
</tr>
</tbody>
</table>
And here's the makeover after some formatting help from `kableExtra`.

![after](images/after.PNG)

It's a nice bundle of functions. It's one that is easy to use and easy to understand; thus, it should be popular for those looking to keep the simplicity of the [Bootstrap](https://www.w3schools.com/bootstrap/bootstrap_tables.asp) table style while adding flexible customization. It's better suited for model-output documents and statistical result-type tables, rather than for fully leveraging on the power of visualization to help interpret the data.

**Package description**

> "Build complex HTML or 'LaTeX' tables using 'kable()' from 'knitr' and the piping syntax from 'magrittr'. Function 'kable()' is a light weight table generator coming from 'knitr'. This package simplifies the way to manipulate the HTML or 'LaTeX' codes generated by 'kable()' and allows users to construct complex tables and customize styles using a readable syntax."

**Process**

The source of the industry data comes from the Bureau of Labor Statistics [QCEW program](https://www.bls.gov/cew/datatoc.htm). The two CSV input files are zipped up and located in the `/data/allfiles` subfolder. One is Las Vegas data and the other is national data. This mini-project is for 2016 annual averages only, but other years are available (`/data/allfilesMASTER`).

Files in the `/script` subfolder serve different purposes.

-   *(1script-merge-allfiles.R)*: Merges the two CSV input files for the chosen year into a single dataframe.
-   *(2script-lq-model.R)*: Calculates and models the LQs into a table layout.
-   *(3script-lq-table.Rmd)*: Runs the `knitr::kable` and `kableExtra` functions to format the table. This is also the output since it's intended for conversions from R Markdown to Markdown and HTML.

![process](images/process.PNG)
