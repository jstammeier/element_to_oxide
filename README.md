# element_to_oxide

Example data must be saved as .txt using tabstop as delimiter. 
First row must contain element names, either as Oxide or as Element. 
Second row must contain the concentration range as "%" or as "ppm". 
(currently no other ranges possible - needs fixing?).  
Empty cells are ok, no stings such as NA, NAN, ND, ...

runApp("app.R")

Test with: 
-Example-data.csv

Requires: 
Element_to_oxide_Äpp.R

runGitHub( "element_to_oxide", "jstammeier")
