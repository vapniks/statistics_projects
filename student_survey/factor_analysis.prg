' Reduce number of variables using factor analysis

group uni_imp Q65 Q66 Q67 Q68 Q69 Q70 Q71 Q72 Q73 Q74 Q75 Q76 Q77 Q78 Q79 Q80 Q81 Q82 
'group uni_impB Q65b Q66b Q67b Q68b Q69b Q70b Q71b Q72b Q73b Q74b Q75b Q76b Q77b Q78b Q79b Q80b Q81b Q82b Q83b
group country_imp Q86 Q87 Q88 Q89 Q90 Q91 Q92 Q93 Q94 
'group country_impB Q86b Q87b Q88b Q89b Q90b Q91b Q92b Q93b Q94b Q95b
group subject_des Q188 Q189 Q190 Q191 Q192 Q193 Q194 Q195 Q196 Q197 Q198 Q199 Q200 Q201 Q202 Q203 Q204 Q205 Q206 Q207
group subject_app Q215 Q216 Q217 Q218 Q219 Q220 Q221 Q222 Q223 Q224 Q225 Q226 Q227 Q228 Q229 Q230 Q231 Q232 Q233 Q234
group sources Q43 Q44 Q45 Q46 Q47 Q48 Q49 Q50 Q51 Q52 Q53 Q54 Q55 Q56 Q57
group other_locations Q8 Q9 Q10 Q11 Q12 Q13 Q14 Q15 Q16 Q17 Q18 Q19 Q20 Q21 Q22 Q23 Q24 Q25 Q26 Q27
group internet Q180 Q181 Q182 Q183 Q184 Q185
group criteria Q122 Q123 Q124 Q125 Q126 Q127 Q128 Q129 Q130 Q131 Q132 Q133 Q134 Q135 Q136 Q137
group uk_uni_factor Q144 Q145 Q146 Q147 Q148 Q149 Q150 Q151 Q152 Q153 Q154 Q155 Q156 Q157 Q158 Q159
group uk_factor Q161 Q162 Q163 Q164 Q165 Q166 Q167 Q168 Q169 Q170 Q171 Q172
group other_modes Q37 Q38 Q39 Q40
group uk_whynot q141 q142 q143

' Create factors using maximum likelihood (ml) and minimum average partial (map) method for choosing number of factors
factor factor1.ml(n=map) uni_imp country_imp subject_des subject_app sources other_locations internet criteria uk_uni_factor uk_factor other_modes uk_whynot

' Save the factor scores
factor1.makescores(n=fact1)

' Perform binary response regression analysis with factors as independent variables
' (d=x: gompit model, h:heteroskedastic error terms)
equation reg_fac1.binary(d=x,h) Q5 C Q214b f1 f2 f3




