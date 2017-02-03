library(Rmisc)
library(ggplot2)
library(plyr)
library(scales)
library(gsubfn)
require(data.table)
require(gridExtra)

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
	POS = c(1,0)

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
	DELIM = ";"
	) {

#PLOT_MEMORY=TRUE

PLOT_SOLVED = !PLOT_SIZE
	
stopifnot(PLOT_SOLVED != PLOT_SIZE)

SIGNS = c(2,16,3,5,9,15) #,6,18,5)

cx <- c("#E69F00", "#56B4E9", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
names(cx) <- c("Clasp", "DynASP(INC)", "DynASP(PRIM)", "Cachet", "DepQBF0", "SharpSAT")
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

df <- df[ with(df, grepl(paste0(paste0(".*", FILTER_STRING), ".*"), instance)), ]
#df <- df[ with(df, !grepl(".*usc.*", algorithm)), ]
		
if (WTF_MODE)
	df2 <- ddply(df, .(solver, algorithm, instance, seed, timeout), summarize, 
				mode = paste0(solver, algorithm), 
				time = as.numeric(as.character(solve_user_runtm)) + as.numeric(as.character(solve_sys_runtm)) + 0.1,			
				size = instance, #ifelse (PLOT_SIZE && FILTER_TREEWIDTH != 0, as.numeric(strapplyc(as.character(instance), paste0(paste0(".+_", as.character(FILTER_TREEWIDTH)), "_([0-9]+)_.+"))), instance),
				ram = as.numeric(as.character(solve_mem)) / 1024) else {
if (DYNASP_CPU_TIME) {
	if (PLOT_SIZE && FILTER_TREEWIDTH != 0) {
		df2 <- ddply(df, .(solver, algorithm, instance, seed, timeout), summarize, 
			mode = paste0(solver, algorithm),
			time = (as.numeric(as.character(solve_user_runtm)) + as.numeric(as.character(solve_sys_runtm)) + 0.1),
			size = as.numeric(strapplyc(as.character(instance), paste0(paste0(".+_", as.character(FILTER_TREEWIDTH)), "_([0-9]+)_.+"))),
			ram = as.numeric(as.character(solve_mem)) / 1024)
	} else {
		df2 <- ddply(df, .(solver, algorithm, instance, seed, timeout), summarize, 
			mode = paste0(solver, algorithm),
			time = (as.numeric(as.character(solve_user_runtm)) + as.numeric(as.character(solve_sys_runtm)) + 0.1),
			size = instance,
			ram = as.numeric(as.character(solve_mem)) / 1024) 
	}
} else {
	if (PLOT_SIZE && FILTER_TREEWIDTH != 0) {
		df2 <- ddply(df, .(solver, algorithm, instance, seed, timeout), summarize, 
			mode = paste0(solver, algorithm),
			time = ifelse (solver == "DynASP", (as.numeric(as.character(internal_runtm)) + 0.1), (as.numeric(as.character(solve_user_runtm)) + as.numeric(as.character(solve_sys_runtm)) + 0.1)),
			size = as.numeric(strapplyc(as.character(instance), paste0(paste0(".+_", as.character(FILTER_TREEWIDTH)), "_([0-9]+)_.+"))),
			ram = as.numeric(as.character(solve_mem)) / 1024)
	} else {
		df2 <- ddply(df, .(solver, algorithm, instance, seed, timeout), summarize, 
			mode = paste0(solver, algorithm),
			time = ifelse (solver == "DynASP", (as.numeric(as.character(internal_runtm)) + 0.1), (as.numeric(as.character(solve_user_runtm)) + as.numeric(as.character(solve_sys_runtm)) + 0.1)),
			size = instance,
			ram = as.numeric(as.character(solve_mem)) / 1024
			)
	}
}	}

df2 <- ddply(df2, .(mode, size), summarize, # instance), summarize, 
			mean = mean(time),
			toa = mean(timeout),
			#lower = quantile(time)[2], #CI(time, ci = CI)[[3]], 
			#upper = quantile(time)[4], #CI(time, ci = CI)[[1]],
			ram_mean = mean(ram) #, 
			#ram_lower = quantile(time)[2], #CI(ram, ci = CI)[[3]], 
			#ram_upper = quantile(time)[4] #CI(ram, ci = CI)[[1]]
			)
df_res <- ddply(df2, .(mode), summarize, # instance), summarize, 
			mean_total = mean(mean), 
			tos = sum(floor(toa+0.5))
			)
			
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
		geom_point(aes(colour = mode, shape = mode)) + #, size = 1.0) +
		#geom_errorbar(aes(colour = mode, ymin = lower, ymax = upper), width = 0.4, size = 0.4) +
		scale_shape_manual(values = SIGNS) +
		scale_colour_manual(values = cx)

	
if (LOG_SCALE)
		p <- p +
		#scale_y_continuous(trans = log10_trans(), breaks = trans_breaks("log10", function(x) 10^x),
         #               labels = trans_format("log10", math_format(10^.x))) # + # + coord_trans(y="log2") +
			
		scale_y_continuous(trans = log10_trans(), breaks = c(0,0.1,1,10,60,120,300,600,1200,2400,4800),# trans_breaks("log10", function(x) 10^x),
                        labels = c(0,0.1,1,10,60,120,300,600,1200,2400,4800))# trans_format("log10", math_format(10^.x))) # + # + coord_trans(y="log2") +
			
			
p <- p + labs(title = TITLE) +
		ylab("CPU time [s]") + 
		xlab(if (PLOT_SOLVED) "number of solved instances" else "instance size") +
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
df_res
}

POS = c(1,0)
IN = "qbf.csv"
OUT = "random_grid_qbf"
lineplot(IN, OUT, 3, FALSE, FALSE, "", TRUE, "2ASP/2QBF-TGrid")
OUT = "random_grid_qbf"
lineplot(IN, OUT, 3, TRUE, FALSE, "", TRUE, "2ASP/2QBF-TGrid")
OUT = "random_grid_qbf"
lineplot(IN, OUT, 3, FALSE, FALSE, "", FALSE, "2ASP/2QBF-TGrid")
OUT = "random_grid_qbf"
lineplot(IN, OUT, 3, TRUE, FALSE, "", FALSE, "2ASP/2QBF-TGrid")

IN = "sat.csv"
POS = c(1,0)
OUT = "random_grid_sat"
lineplot(IN, OUT, 3, FALSE, FALSE, "", FALSE, "ASP/SAT-TGrid")
OUT = "random_grid_sat"
lineplot(IN, OUT, 3, TRUE, FALSE, "", FALSE, "ASP/SAT-TGrid")
OUT = "random_grid_sat"
lineplot(IN, OUT, 3, FALSE, FALSE, "", TRUE, "ASP/SAT-TGrid")
OUT = "random_grid_sat"
lineplot(IN, OUT, 3, TRUE, FALSE, "", TRUE, "ASP/SAT-TGrid")

POS = c(0,1)
IN = "real_ds.csv"
OUT = "real_world_ds"
lineplot(IN, OUT, 0, FALSE, FALSE, "", TRUE, "Ds")
OUT = "real_world_ds"
lineplot(IN, OUT, 0, TRUE, FALSE, "", TRUE, "Ds")
OUT = "real_world_ds"
lineplot(IN, OUT, 0, TRUE, FALSE, "", FALSE, "Ds")
OUT = "real_world_ds"
lineplot(IN, OUT, 0, FALSE, FALSE, "", FALSE, "Ds")

IN = "real_3col.csv"
OUT = "real_world_3col"
lineplot(IN, OUT, 0, FALSE, FALSE, "", TRUE, "3Col")
OUT = "real_world_3col"
lineplot(IN, OUT, 0, TRUE, FALSE, "", TRUE, "3Col")
OUT = "real_world_3col"
lineplot(IN, OUT, 0, FALSE, FALSE, "", FALSE, "3Col")
OUT = "real_world_3col"
lineplot(IN, OUT, 0, TRUE, FALSE, "", FALSE, "3Col")

IN = "real_2col.csv"
OUT = "real_world_2col"
lineplot(IN, OUT, 0, FALSE, FALSE, "2col_crafted", TRUE, "2Col")
OUT = "real_world_2col"
lineplot(IN, OUT, 0, TRUE, FALSE, "2col_crafted", TRUE, "2Col")
OUT = "real_world_2col"
lineplot(IN, OUT, 0, TRUE, FALSE, "2col_crafted", FALSE, "2Col")
OUT = "real_world_2col"
lineplot(IN, OUT, 0, FALSE, FALSE, "2col_crafted", FALSE, "2Col")

IN = "real_stein.csv"
OUT = "real_world_steiner"
lineplot(IN, OUT, 0, FALSE, FALSE, "", TRUE, "St")
OUT = "real_world_steiner"
lineplot(IN, OUT, 0, TRUE, FALSE, "", TRUE, "St")
OUT = "real_world_steiner"
lineplot(IN, OUT, 0, FALSE, FALSE, "", FALSE, "St")
OUT = "real_world_steiner"
lineplot(IN, OUT, 0, TRUE, FALSE, "", FALSE, "St")

IN = "real_vc_plain.csv"
OUT = "real_vc_plain"
lineplot(IN, OUT, 0, FALSE, FALSE, "", TRUE, "cVc")
OUT = "real_vc_plain"
lineplot(IN, OUT, 0, TRUE, FALSE, "", TRUE, "cVc")
OUT = "real_vc_plain"
lineplot(IN, OUT, 0, FALSE, FALSE, "", FALSE, "cVc")
OUT = "real_vc_plain"
lineplot(IN, OUT, 0, TRUE, FALSE, "", FALSE, "cVc")

IN = "real_vc_submin.csv"
OUT = "real_vc_submin"
lineplot(IN, OUT, 0, FALSE, FALSE, "", TRUE, "sVc")
OUT = "real_vc_submin"
lineplot(IN, OUT, 0, TRUE, FALSE, "", TRUE, "sVc")
OUT = "real_vc_submin"
lineplot(IN, OUT, 0, FALSE, FALSE, "", FALSE, "sVc")
OUT = "real_vc_submin"
lineplot(IN, OUT, 0, TRUE, FALSE, "", FALSE, "sVc")

IN = "real_clique.csv"
OUT = "real_world_clique"
lineplot(IN, OUT, 0, FALSE, FALSE, "", TRUE, "Cl")
OUT = "real_world_clique"
lineplot(IN, OUT, 0, TRUE, FALSE, "", TRUE, "Cl")
OUT = "real_world_clique"
lineplot(IN, OUT, 0, FALSE, FALSE, "", FALSE, "Cl")
OUT = "real_world_clique"
lineplot(IN, OUT, 0, TRUE, FALSE, "", FALSE, "Cl")
