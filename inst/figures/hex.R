imgurl <- file.path("inst/figures/austraits_logo_green_transparent.png")
hexSticker::sticker(imgurl, package="austraits", 
                    p_color = "white",
                    p_size=32,
                    p_y = 1.05,
                    s_x=.94, s_y=1.02,
                    s_width=.75, s_height = .75,
                    h_fill = "chartreuse4", h_color = "white",
                    filename="inst/figures/austraits_hex.png")

# SVG
hexSticker::sticker(imgurl, package="austraits", 
                    p_color = "white",
                    p_size=10,
                    p_y = 1.05,
                    s_x=.94, s_y=1.02,
                    s_width=.75, s_height = .75,
                    h_fill = "chartreuse4", h_color = "white",
                    filename="inst/figures/austraits_hex.svg")

#PDF
imgurl <- file.path("inst/figures/austraits_logo_green_transparent.png")
hexSticker::sticker(imgurl, package="austraits", 
                    p_color = "white",
                    p_size=10,
                    p_y = 1.05,
                    s_x=.94, s_y=1.02,
                    s_width=.75, s_height = .75,
                    h_fill = "chartreuse4", h_color = "white",
                    white_around_sticker = FALSE,
                    filename="inst/figures/austraits_hex.pdf")  -> p

ggsave('austraits_hex.pdf', p, bg='transparent')

