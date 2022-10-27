imgurl <- file.path("inst/figures/austraits_logo_green_transparent.png")
hexSticker::sticker(imgurl, package="austraits", 
                    p_color = "white",
                    p_size=30,
                    p_y = 1,
                    s_x=.94, s_y=1,
                    s_width=.75, s_height = .75,
                    h_fill = "chartreuse4", h_color = "white",
                    filename="inst/figures/austraits_hex.png")
