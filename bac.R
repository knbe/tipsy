options(warn = -1)

# variables
v = 300 	# vol in ml
z = 5.0 / 100 	# abv / 100
#t = 3		# hours since most recent drinking sess

# constants
a = 1 		# proportion of alcohol absorbed
d = 0.789 	# density of alcohol

# basically constants but not really
rho = 0.76	# depends on bmi (average for v and a)
M = 50.0	# mass of a in kg
beta_lo = 15	# elimination rate mg% per hour or g/ml/hr 
beta_hi = 20  	# upper bound (consumed food)

delta_rho = 0.07 # (if) I would say (so myself)
delta_M = 5

denom_upp = rho * M + 
	delta_rho * M + 
	delta_M * rho + 
	delta_rho * delta_M

denom_low = rho * M - 
	delta_rho * M - 
	delta_M * rho + 
	delta_rho * delta_M

### test data ## 
#
## variables
#v = 3550 	# vol in ml
#z = 4.0 / 100 	# abv / 100
##t = 3		# hours since most recent drinking sess
#
## constants
#a = 1 		# proportion of alcohol absorbed
#d = 0.789 	# density of alcohol
#
## basically constants but not really
#rho = 0.73	# depends on bmi (average for v and a)
#M = 81.6	# mass of a in kg
#beta_lo = 14.8	# elimination rate mg% per hour or g/ml/hr 
#beta_hi = 20  # upper bound (consumed food)

vzad = 100 * v * z * a * d

alpha = vzad / (rho * M)

alpha_low = vzad / denom_upp
alpha_up = vzad / denom_low

alpha_diff_up = (alpha_up - alpha) / 1000
alpha_diff_lo = (alpha - alpha_low) / 1000

#alpha_diff_up
#alpha_diff_lo

C = function(beta, t) {
	(alpha - beta * t) / 1000
}

#C(beta_lo, t)
#C(beta_hi, t)

t = seq(1.5, 6, 0.5)
C_lo = C(beta_lo, t)
C_hi = C(beta_hi, t)

#plot = plot(t, C_t)

library(ggplot2)

data = data.frame(t = t,
		  C_lo = C_lo,
		  C_lo_low = C_lo - alpha_diff_lo,
		  C_lo_upp = C_lo + alpha_diff_up,
		  C_hi = C_hi, 
		  C_hi_low = C_hi - alpha_diff_lo,
		  C_hi_upp = C_hi + alpha_diff_up)

plot = ggplot(data = data) +
	geom_line(aes(x = t, y = C_lo)) +
	geom_point(aes(x = t, y = C_lo) ) +
	geom_line(aes(x = t, y = C_hi, color = 'red')) +
	geom_point(aes(x = t, y = C_hi, color = 'red')) +
	geom_segment(aes(x = t, xend = t, 
			 y = C_lo_low, yend = C_lo_upp)) +
#	geom_segment(aes(x = t, xend = t, 
#			 y = C_hi_low, yend = C_hi_upp), 
#		     color = 'red') +
	ylab("C") +
	scale_y_continuous(limits = c(0, 0.12), 
			   breaks = seq(0, 0.12, 0.01)) +
	ggtitle("bac") +
	theme_bw()

ggsave("plot.png", plot)
