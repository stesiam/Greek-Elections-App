htmlTemplate("template.html",
             button = actionButton("action", "Calculate !", class = "btn-primary"),
             
             slider_nd = sliderInput("nd", "", 25, 50 , 39.9, step = 0.1),
             slider_syriza = sliderInput("syriza", "", 20, 45 , 31,5, step = 0.1),
             slider_pasok = sliderInput("pasok", "", 5, 15 , 8.1, step = 0.1),
             slider_kke = sliderInput("kke", "", 1, 10, 5.3, step = 0.1),
             slider_ellisi = sliderInput("ellisi", "",  1, 10 , 3.7, step = 0.1),
             slider_mera25 = sliderInput("mera25", "",  1, 10 , 3.4, step = 0.1),
             slider_other1 = sliderInput("other1", "",  1, 10, 1, step = 0.1),
             slider_other2 = sliderInput("other2", "",  1, 10, 1, step = 0.1),
             slider_other3 = sliderInput("other3", "",  1, 10, 1, step = 0.1),
)
