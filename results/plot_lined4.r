library(Rmisc)
library(ggplot2)
library(plyr)
library(scales)
library(gsubfn)
require(data.table)
require(gridExtra)

#NOTE: READ @NOTE below!

FILTER_TREEWIDTH = 0
	DYNASP_CPU_TIME = FALSE
	WTF_MODE = FALSE
	FILTER_STRING = ""
	PLOT_CUM = FALSE
	PLOT_SIZE = FALSE
	LOG_SCALE = TRUE
	PLOT_MEMORY = FALSE
	DELIM = ";"
	TITLE = "plot"
	POS = c(0,1)
	CUSTOM_SCALE = TRUE

lineplot <- function(
	IN, OUT, 
	FILTER_TREEWIDTH = 0,
	DYNASP_CPU_TIME = FALSE,
	WTF_MODE = FALSE,
	FILTER_STRING = "",
	PLOT_CUM = FALSE, TITLE = "plot",
	PLOT_SIZE = FALSE,
	LOG_SCALE = TRUE,
	PLOT_MEMORY = FALSE,
	DELIM = ";",
	CUSTOM_SCALE = TRUE
	) {

#PLOT_MEMORY=TRUE

PLOT_SOLVED = !PLOT_SIZE
	
stopifnot(PLOT_SOLVED != PLOT_SIZE)

SIGNS = c(2,16,3,8,5) #,6,18,5)

#cx <- c("#E69F00", "#56B4E9", "#009E73")#, "#F0E442", "#0072B2", "#D55E00")
#names(cx) <- c("ASPARTIX", "CEGARTIX", "D-FLAT^2")#, "monolithic_semi", "seminorm_pref", "seminorm_semi")


#cx <- c("#E69F00", "#56B4E9", "#F0E442", "#F0E442", "#0072B2")
#cx <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#F0E442")#, "#D55E00", "#CC79A7")
cx <- c("#E69F00", "#56B4E9", "#F0E442", "#009E73", "#0072B2")
names(cx) <- c("Clasp", "DynASP 2(INC)", "DynASP 2(PRIM)", "DynASP 2.5(PRIM)", "DynASP 2.5(INC)")

names(SIGNS) <- names(cx)


if (PLOT_SOLVED)
	OUT = paste0(OUT, "_solved") else
if (PLOT_SIZE) {
	if (FILTER_TREEWIDTH != 0)
		OUT = paste0(OUT, "_size")
	else
		OUT = paste0(OUT, "_inst")
}

if (PLOT_MEMORY)
	OUT = paste0(OUT, "_mem")

if (PLOT_CUM)
	OUT = paste0(OUT, "_cum")

if (LOG_SCALE)
	OUT = paste0(OUT, "_log")
	
if (DYNASP_CPU_TIME)
	OUT = paste0(OUT, "_cpu")

if (FILTER_TREEWIDTH > 0)
	OUT = paste0(OUT, paste0("_tw", as.character(FILTER_TREEWIDTH)))

if (PLOT_MEMORY)
	cairo_pdf(paste0(OUT, ".pdf"), height = 6, width = 15) else
cairo_pdf(paste0(OUT, ".pdf"), height = 6, width = 7.5)


df <- read.table(IN, header = TRUE, sep = DELIM)

#PREFILTER
if (FILTER_TREEWIDTH != 0)
	df <- df[ with(df, grepl(paste0(paste0(".*_", as.character(FILTER_TREEWIDTH)), "_.*"), instance)), ]

df <- df[ with(df, grepl(paste0(paste0(".*", FILTER_STRING), ".*"), solver)), ]


#@NOTE: UNCOMMENT THIS LINE FOR SHOWING AVG DETAILS AMONG SOLVED INSTANCES!
#df <- df[ with(df, grepl("0", timeout)), ]



#print(df)

#df <- df[ with(df, !grepl(".*usc.*", algorithm)), ]
		
if (WTF_MODE)
	df2 <- ddply(df, .(solver, algorithm, instance, seed, timeout), summarize, 
				mode = paste0(solver, ""),#algorithm), 
				time = as.numeric(as.character(solve_user_runtm)) + 0.1,			
				pass2time = as.numeric(as.character(pass2_time)) + 0,
				pass1time = as.numeric(as.character(vars))+0,
				pass3time = as.numeric(as.character(pass3_time))+0,
				decomptime = as.numeric(as.character(decomp_time))+0,
				size = paste0(algorithm,instance), #ifelse (PLOT_SIZE && FILTER_TREEWIDTH != 0, as.numeric(strapplyc(as.character(instance), paste0(paste0(".+_", as.character(FILTER_TREEWIDTH)), "_([0-9]+)_.+"))), instance),
				twa = ifelse(is.nan(as.numeric(as.character(tw))), 15, as.numeric(as.character(tw))),
				ram = as.numeric(as.character(solve_mem)) / 1024) else {
if (DYNASP_CPU_TIME) {
	if (PLOT_SIZE && FILTER_TREEWIDTH != 0) {
		df2 <- ddply(df, .(solver, algorithm, instance, seed, timeout), summarize, 
			mode = paste0(solver, ""),# algorithm),
			time = (as.numeric(as.character(solve_user_runtm)) +  0.1),
			pass2time = as.numeric(as.character(pass2_time)) + 0,
			pass1time = as.numeric(as.character(vars))+0,
			pass3time = as.numeric(as.character(pass3_time))+0,
			decomptime = as.numeric(as.character(decomp_time))+0,
			size = as.numeric(strapplyc(as.character(instance), paste0(paste0(".+_", as.character(FILTER_TREEWIDTH)), "_([0-9]+)_.+"))),
			twa = ifelse(is.nan(as.numeric(as.character(tw))), 15, as.numeric(as.character(tw))),
			ram = as.numeric(as.character(solve_mem)) / 1024)
	} else {
		df2 <- ddply(df, .(solver, algorithm, instance, seed, timeout), summarize, 
			mode = paste0(solver, ""),# algorithm),
			time = (as.numeric(as.character(solve_user_runtm)) + 0.1),
			pass2time = as.numeric(as.character(pass2_time)) + 0,
			pass1time = as.numeric(as.character(vars))+0,
			pass3time = as.numeric(as.character(pass3_time))+0,
			decomptime = as.numeric(as.character(decomp_time))+0,
			size = paste0(algorithm,instance),
			twa = ifelse(is.nan(as.numeric(as.character(tw))), 15, as.numeric(as.character(tw))),
			ram = as.numeric(as.character(solve_mem)) / 1024) 
	}
} else {
	if (PLOT_SIZE && FILTER_TREEWIDTH != 0) {
		df2 <- ddply(df, .(solver, algorithm, instance, seed, timeout), summarize, 
			mode = paste0(solver, ""),# algorithm),
			time = ifelse (solver == "clasp-banane", (as.numeric(as.character(solve_user_runtm)) +  0), ifelse(grepl("dynasp_le",solver), ifelse(is.nan(as.numeric(as.character(vars))), 1200, as.numeric(as.character(vars))), ifelse(is.nan(as.numeric(as.character(pass3_time))), 1200, (as.numeric(as.character(pass2_time)) + as.numeric(as.character(vars)) + as.numeric(as.character(pass3_time)) + 0))))  +  0.1,
			pass2time = as.numeric(as.character(pass2_time)) + 0,
			pass1time = as.numeric(as.character(vars))+0,
			pass3time = as.numeric(as.character(pass3_time))+0,
			decomptime = as.numeric(as.character(decomp_time))+0,
			size = as.numeric(strapplyc(as.character(instance), paste0(paste0(".+_", as.character(FILTER_TREEWIDTH)), "_([0-9]+)_.+"))),
			twa = ifelse(is.nan(as.numeric(as.character(tw))), 15, as.numeric(as.character(tw))),
			ram = as.numeric(as.character(solve_mem)) / 1024)
	} else {
		df2 <- ddply(df, .(solver, algorithm, instance, seed, timeout), summarize, 
			mode = paste0(solver, ""),# algorithm),
			time = ifelse (solver == "clasp-banane", (as.numeric(as.character(solve_user_runtm)) +  0), ifelse(grepl("dynasp_le",solver), ifelse(is.nan(as.numeric(as.character(vars))), 1200, as.numeric(as.character(vars))), ifelse(is.nan(as.numeric(as.character(pass3_time))), 1200, (as.numeric(as.character(pass2_time)) + as.numeric(as.character(vars)) + as.numeric(as.character(pass3_time)) + 0))))  +  0.1,
			pass2time = as.numeric(as.character(pass2_time)) + 0,
			pass1time = as.numeric(as.character(vars))+0,
			pass3time = as.numeric(as.character(pass3_time))+0,
			decomptime = as.numeric(as.character(decomp_time))+0,
			size = paste0(algorithm,instance),
			twa = ifelse(is.nan(as.numeric(as.character(tw))), 15, as.numeric(as.character(tw))),
			ram = as.numeric(as.character(solve_mem)) / 1024
			)
	}
}	}

df2 <- ddply(df2, .(mode, size), summarize, # instance), summarize, 
			mean = mean(time),
			dtime = mean(decomptime),
			p1mean = mean(pass1time),
			p2mean = mean(pass2time),
			p3mean = mean(pass3time),
			toa = floor(mean(timeout)+0.0),
			meantw = mean(twa),
			#lower = quantile(time)[2], #CI(time, ci = CI)[[3]], 
			#upper = quantile(time)[4], #CI(time, ci = CI)[[1]],
			ram_mean = mean(ram) #, 
			#ram_lower = quantile(time)[2], #CI(ram, ci = CI)[[3]], 
			#ram_upper = quantile(time)[4] #CI(ram, ci = CI)[[1]]
			)
#print (df2)
df_res <- ddply(df2, .(mode), summarize, # instance), summarize, 
			mean_total = mean(mean),
			mean_dtime = mean(dtime),
			mean_p1mean = mean(p1mean),
			mean_p2mean = mean(p2mean),
			mean_p3mean = mean(p3mean), 
			mean_totaltw = mean(meantw),
			stddev = sd(meantw),
			tos = sum(floor(toa+0.5))
			)
print(df_res)
#return
			
#pad to 0 if conf. interval goes below 0
#for (i in 1:length(df2$lower)) {
#	df2$lower[i] = max(0, df2$lower[i])
#	df2$ram_lower[i] = max(0, df2$ram_lower[i])
#}

#@FOR (CACTUS) {
if (PLOT_SIZE) {
	df2 <- df2[with (df2, order(mode, size, mean)), ]
	ram_df2 <- df2[with (df2, order(mode, size, ram_mean)), ]
} else {
	df2 <- df2[with (df2, order(mode, mean)), ]
	ram_df2 <- df2[with (df2, order(mode, ram_mean)), ]
}

j = 1
for (i in 1:length(df2$mean)) {
	if (i > 1 && df2$mode[i-1] != df2$mode[i]) {
		j = 1
	}
	df2$id[i] = i
	ram_df2$id[i] = i
	df2$solved[i] = j
	ram_df2$solved[i] = j
	j = j + 1
}

dt <- data.table(df2)
ram_dt <- data.table(ram_df2)

if (PLOT_SIZE) {
	dtx <- dt[, cumsum := cumsum(mean), by = list(mode, size)]
	ram_dtx <- ram_dt[, cumsum := cumsum(ram_mean), by = list(mode, size)]
} else {
	dtx <- dt[, cumsum := cumsum(mean), by = list(mode)]
	ram_dtx <- ram_dt[, cumsum := cumsum(ram_mean), by = list(mode)]
}
#@FOR (CACTUS) }


p <- 	ggplot(dtx, aes(x = if (PLOT_SOLVED) solved else size, y = if (PLOT_CUM) cumsum else mean, group = mode)) + 
		geom_point(aes(colour = mode, shape = mode)) #+ #, size = 1.0) +
		#geom_errorbar(aes(colour = mode, ymin = lower, ymax = upper), width = 0.4, size = 0.4) +

if (CUSTOM_SCALE)
		p <- p +
		scale_shape_manual(values = SIGNS) +
		scale_colour_manual(values = cx)

	
if (LOG_SCALE)
		p <- p +
		#scale_y_continuous(trans = log10_trans(), breaks = trans_breaks("log10", function(x) 10^x),
         #               labels = trans_format("log10", math_format(10^.x))) # + # + coord_trans(y="log2") +
			
		scale_y_continuous(trans = log10_trans(), breaks = c(0,0.1,1,10,60,120,300,600,1200,2400,4800),# trans_breaks("log10", function(x) 10^x),
                        labels = c(0,0.1,1,10,60,120,300,600,1200,2400,4800))# trans_format("log10", math_format(10^.x))) # + # + coord_trans(y="log2") +
			
			
#p<- p +		
#labs(title = "") +
#		ylab("") + 
#		xlab("") +

p <- p + labs(title = TITLE) +
		ylab("CPU time [s]") + 
		xlab(if (PLOT_SOLVED) "solved instances" else "instance size") +

theme(legend.position = POS, 
				legend.justification = POS,
				legend.title = element_blank(),
				legend.text = element_text(size = 16),
				legend.key = element_rect(fill="gray90", colour="gray90"),
				legend.background = element_rect(fill="gray90", size=.5),
				text = element_text(size = 26),
				panel.background = element_rect(fill = 'gray90'),#, colour = 'black'),
				panel.grid.major.x = element_blank(),
				panel.grid.major.y = element_line(size = .1, color = "gray"),
				axis.text.y = element_text(colour = "black"),
				axis.text.x = element_text(colour = "black"),	   
				#legend.justification = c(-1.2, 1), text = element_text(size=25),
				axis.title.y = element_text(vjust = 0, hjust = 0.5),
				axis.title.x = element_text(vjust = 0, hjust = 0.5),
				plot.margin = unit(c(1, 2, 1, 0), "mm")) #+ labs(fill = "")
		

#p2 <- 	ggplot(ram_dtx, aes(x = if (PLOT_SOLVED) solved else size, y = if (PLOT_CUM) cumsum else ram_mean, group = mode)) + 
#		geom_point(aes(colour = mode, shape = mode)) + #, size = 1.0) +
#		#geom_errorbar(aes(colour = mode, ymin = lower, ymax = upper)) + #, width = 0.4, size = 0.4) +
#		scale_shape_manual(values = SIGNS) + labs(title="asdf")

#if (LOG_SCALE)
#	p2 <- p2 +	scale_y_continuous(trans = log10_trans(), breaks = trans_breaks("log10", function(x) 10^x),
#                       labels = trans_format("log10", math_format(10^.x)))
						
#p2 <- p2 +						
#		ylab("resident set [MB]") +
#		xlab(if (PLOT_SOLVED) "number of solved instances" else "instance size")
#		theme(legend.position = POS,
#				legend.justification = POS,
#				legend.title = element_blank(),
#				text = element_text(size = 26),
#				#panel.background = element_rect(fill = 'white', colour = 'black'),
#				panel.grid.major.x = element_blank(),
#				panel.grid.major.y = element_line(size = .1, color = "gray"),
#				axis.text.y = element_text(colour = "black"),
#				axis.text.x = element_text(colour = "black"),	   
#				#legend.justification = c(-1.2, 1), text = element_text(size=25),
#				#axis.title.y = element_text(vjust = 5.65, hjust = -1.98),
#				axis.title.x = element_text(vjust = -500.0),
#				plot.margin = unit(c(7, 7, 1, -1), "mm"))
				
				

#if (PLOT_MEMORY)
#	grid.arrange(p, p2, ncol = 2) else
grid.arrange(p, ncol = 1)

#this saves png in your current directory
dev.off()		
#print(df_res)
}

problems <- c("_steiner14")
for(val in 1:length(problems)) 
{
	elem = problems[val]
	print(elem)
	PTITLE = elem
	IN = elem
	
	#OUT = PTITLE
	#lineplot(IN, OUT, 0, TRUE, FALSE, "(DynASP.*)", TRUE, elem, FALSE, TRUE, FALSE, ";", FALSE)
	OUT = PTITLE
	#lineplot(IN, OUT, 0, TRUE, FALSE, "_n_999", FALSE, elem, FALSE, TRUE, FALSE, ";", FALSE)
	lineplot(IN, OUT, 0, TRUE, FALSE, "(clasp|dynasp_6_r_3_2|dynasp_6_r_3_0|dynasp_6_r_3_1)", FALSE, elem, FALSE, TRUE, FALSE, ";", FALSE)
	lineplot(IN, OUT, 0, TRUE, FALSE, "(clasp|dynasp_6_r_3_2|dynasp_6_r_3_0|dynasp_6_r_3_1)", FALSE, elem, FALSE, FALSE, FALSE, ";", FALSE)
	lineplot(IN, OUT, 0, FALSE, FALSE, "(clasp|dynasp_6_r_3_2|dynasp_6_r_3_0|dynasp_6_r_3_1)", FALSE, elem, FALSE, TRUE, FALSE, ";", FALSE)
	lineplot(IN, OUT, 0, FALSE, FALSE, "(clasp|dynasp_6_r_3_2|dynasp_6_r_3_0|dynasp_6_r_3_1)", FALSE, elem, FALSE, FALSE, FALSE, ";", FALSE)
	lineplot(IN, OUT, 0, TRUE, FALSE, "(clasp|dynasp_6_r_3_2|dynasp_6_r_3_0|dynasp_6_r_3_1)", TRUE, elem, FALSE, TRUE, FALSE, ";", FALSE)
	lineplot(IN, OUT, 0, TRUE, FALSE, "(clasp|dynasp_6_r_3_2|dynasp_6_r_3_0|dynasp_6_r_3_1)", TRUE, elem, FALSE, FALSE, FALSE, ";", FALSE)
	lineplot(IN, OUT, 0, FALSE, FALSE, "(clasp|dynasp_6_r_3_2|dynasp_6_r_3_0|dynasp_6_r_3_1)", TRUE, elem, FALSE, TRUE, FALSE, ";", FALSE)
	lineplot(IN, OUT, 0, FALSE, FALSE, "(clasp|dynasp_6_r_3_2|dynasp_6_r_3_0|dynasp_6_r_3_1)", TRUE, elem, FALSE, FALSE, FALSE, ";", FALSE)
}
