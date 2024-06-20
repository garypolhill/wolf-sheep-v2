#!/usr/bin/env Rscript

# Defaults

n.runs <- NULL # The default is to plot all the runs
log.xy <- FALSE # Don't plot log axes
max.step <- NULL # The default is to plot all steps
plot.end <- TRUE # Plot end-points
level.steps <- 5 # How many steps to plot using 'levelplot'
expt.name <- NULL # Name for the experiment to put as title (default is dirname)
level.wolves <- NULL # Range (min, max) to use for wolf level plot (default is calculated)
level.sheep <- NULL # Range (min, max) to use for sheep level plit (default is calculated)
cmp.runs <- c() # Other experiment to compare results with
filters <- list() # Allow experiments to be filtered by parameter values

# Logbook

logbook <- function(...) {
    cat("plot-results.R log (", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), "): ", ...,
        "\n", sep = "")
}

# Process command line arguments

dir <- NULL
run.list <- c()
pdf.file <- ""

args <- commandArgs(TRUE)
if(length(args) > 0 && (args[1] == "--help" || args[1] == "--usage")) {
    cat("Usage: ./plot-results.R [--runs <runlist>] [--max-step <t>] ",
        "[--expt-name <title>] [--level-sheep <min> <max>] ",
        "[--level-wolves <min> <max> [--log-xy] ",
        "[--no-plot-end] <dir> [PDF file]\n")
    q(status = 0)
}

while(length(args) > 0 && substr(args[1], 0, 1) == "-") {
    opt <- args[1]
    args <- tail(args, n = -1)

    if(opt == "--log-xy") {
        log.xy <- TRUE
    } else if(opt == "--no-plot-end") {
        plot.end <- FALSE
    } else if(opt == "--max-step") {
        max.step <- as.numeric(args[1])
        args <- tail(args, n = -1)
    } else if(opt == "--cmp") {
        cmp.runs <- c(cmp.runs, args[1])
        args <- tail(args, n = -1)
    } else if(opt == "--expt-name") {
        expt.name <- args[1]
        args <- tail(args, n = -1)
    } else if(opt == "--runs") {
        for(run.id in unlist(strsplit(args[1], ","))) {
            range <- unlist(strsplit(run.id, "-")) 
            if(length(range) == 1) {
                run.list <- c(run.list, range[1])
            } else if(length(range) == 2) {
                l <- nchar(range[1])
                run.min <- as.numeric(range[1])
                run.max <- as.numeric(range[2])
                for(i in run.min:run.max) {
                    run.list <- c(run.list, sprintf("%0*d", l, i))
                }
            }
        }
        n.runs <- length(run.list)
        args <- tail(args, n = -1)
    } else if(opt == "--level-sheep") {
        level.sheep <- c(as.numeric(args[1]), as.numeric(args[2]))
        if(level.sheep[1] >= level.sheep[2]) {
            stop("Minimum level-sheep ", level.sheep[1], "must be less than maximum",
                level.sheep[2], call. = FALSE)
        }
        args <- tail(args, n = -2)
    } else if(opt == "--level-wolves") {
        level.wolves <- c(as.numeric(args[1]), as.numeric(args[2]))
        if(level.wolves[1] >= level.wolves[2]) {
            stop("Minimum level-wolves ", level.wolves[1],
                " must be less than maximum ", level.wolves[2], call. = FALSE)
        }
        args <- tail(args, n = -2)
    } else if(opt == "--filter") {
        filters[[args[1]]] <- args[2]
        args <- tail(args, n = -2)
    } else {
        stop("Option ", opt, " not recognized. Try --help for usage.", call. = FALSE)
    }
}

if(length(filters) == 0) {
    filters <- NULL
}

if(length(args) < 1) {
    stop("No directory specified", call. = FALSE)
}

dir <- args[1]

if(!file.exists(dir)) {
    stop("Directory ", dir, " does not exist", call. = FALSE)
}

if(!file.info(dir)$isdir) {
    stop("Directory argument ", dir, " exists, but is not a directory", call. = FALSE)
}

if(is.null(expt.name)) {
    expt.name <- dir
}

if(length(args) > 1) {
    pdf.file <- args[2]
} else {
    pdf.file <- paste0(dir, ".pdf")
}

# Read the directory and get all the run files

run.files <- c()
if(is.null(n.runs)) {
    run.files <- list.files(path = dir, pattern = "*-table.csv", full.names = TRUE)
    if(length(run.files) == 0) {
        stop("No NetLogo run files found in directory ", dir, call. = FALSE)
    }
    n.runs = length(run.files)
} else {
    for(run.id in run.list) {
        pat <- paste0("*-", run.id, "-table.csv")
        mat <- list.files(path = dir, pattern = pat, full.names = TRUE)
        if(length(mat) == 1) {
            run.files <= c(run.files, mat)
        } else {
            stop("Zero or non-unique match for ", pat, " in ", dir, call. = FALSE)
        }
    }
}

logbook("Found ", length(run.files), " NetLogo run files in directory ", dir)

# Take care of the comparison directories -- not going to use run.list so
# the comparison directories will have all the files in

all.files <- c(run.files)
cmp.files <- list()
for(dircmp in cmp.runs) {
    if(!file.exists(dircmp)) {
        stop("Comparison directory ", dircmp, " does not exist", call. = FALSE)
    }
    if(!file.info(dircmp)$isdir) {
        stop("Comparison directory ", dircmp, " exists, but is not a directory", call. = FALSE)
    }

    cmp.files[[dircmp]] <- list.files(path = dircmp, pattern = "*-table.csv", full.names = TRUE)

    logbook("Found ", length(cmp.files[[dircmp]]), " NetLogo run files in comparison directory ", dircmp)

    all.files <- c(all.files, cmp.files[[dircmp]])
}

# First pass through the run files -- find the maximum sheep and wolf populations

max.sheep <- 0
max.sheep.level <- 0
max.wolves <- 0
max.wolves.level <- 0
min.sheep <- NULL
min.sheep.level <- NULL
min.wolves <- NULL
min.wolves.level <- NULL

for(file in all.files) {
    df <- read.csv(file, skip = 6, check.names = FALSE)
    if(!is.null(max.step)) {
        df <- subset(df, `[step]` <= max.step)
    }
    df.level <- subset(df, `[step]` > 0 & `[step]` <= level.steps)
    count.sheep.max <- max(df$`count sheep`)
    count.sheep.min <- min(df$`count sheep`)
    count.sheep.max.level <- max(df.level$`count sheep`)
    count.sheep.min.level <- min(df.level$`count sheep`)
    if(count.sheep.max > max.sheep) {
        max.sheep <- count.sheep.max
    }
    if(is.null(min.sheep) || count.sheep.min < min.sheep) {
        min.sheep <- count.sheep.min
    }
    if(count.sheep.max.level > max.sheep.level) {
        max.sheep.level <- count.sheep.max.level
    }
    if(is.null(min.sheep.level) || count.sheep.min.level < min.sheep.level) {
        min.sheep.level <- count.sheep.min.level
    }

    count.wolves.max <- max(df$`count wolves`)
    count.wolves.min <- min(df$`count wolves`)
    count.wolves.max.level <- max(df.level$`count wolves`)
    count.wolves.min.level <- min(df.level$`count wolves`)
    if(count.wolves.max > max.wolves) {
        max.wolves <- count.wolves.max
    }
    if(is.null(min.wolves) || count.wolves.min < min.wolves) {
        min.wolves <- count.wolves.min
    }
    if(count.wolves.max.level > max.wolves.level) {
        max.wolves.level <- count.wolves.max.level
    }
    if(is.null(min.wolves.level) || count.wolves.min.level < min.wolves.level) {
        min.wolves.level <- count.wolves.min.level
    }
}

logbook("Sheep population in any run is in range [", min.sheep, ", ", max.sheep, "]")
logbook("Wolf population in any run is in range [", min.wolves, ", ", max.wolves, "]")
logbook("Sheep population in steps [1, ", level.steps, "] is in range [",
    min.sheep.level, ", ", max.sheep.level, "]")
logbook("Wolf population in steps [1, ", level.steps, "] is in range [",
    min.wolves.level, ", ", max.wolves.level, "]")

# Apply user requests for level plot axes
if(!is.null(level.sheep)) {
    if(level.sheep[1] > min.sheep.level) {
        stop("Specified minimum sheep level ", level.sheep[1],
            " is too high for data ", min.sheep.level, call. = FALSE)
    }
    if(level.sheep[2] < max.sheep.level) {
        stop("Specified maximum sheep level ", level.sheep[2],
            " is too low for data ", max.sheep.level, call. = FALSE)
    }
    min.sheep.level <- level.sheep[1]
    max.sheep.level <- level.sheep[2]
    logbook("Set sheep population range for plotting steps [1, ", level.steps, "] to [",
        min.sheep.level, ", ", max.sheep.level, "]")
}
if(!is.null(level.wolves)) {
    if(level.wolves[1] > min.wolves.level) {
        stop("Specified minimum wolves level ", level.wolves[1],
            " is too high for data ", min.wolves.level, call. = FALSE)
    }
    if(level.wolves[2] < max.wolves.level) {
        stop("Specified maximum wolves level ", level.wolves[2],
            " is too low for data ", max.wolves.level, call. = FALSE)
    }
    min.wolves.level <- level.wolves[1]
    max.wolves.level <- level.wolves[2]
    logbook("Set wolf population range for plotting steps [1, ", level.steps, "] to [",
        min.wolves.level, ", ", max.wolves.level, "]")
}

# Make the 'level plots' have square cells
diff.sheep.level <- max.sheep.level - min.sheep.level
diff.wolf.level <- max.wolves.level - min.wolves.level
diff.adj <- abs(diff.sheep.level - diff.wolf.level)
diff.adj <- (diff.adj + (diff.adj %% 2)) / 2
if(diff.sheep.level > diff.wolf.level) {
    max.wolves.level <- max.wolves.level + diff.adj
    if(min.wolves.level - diff.adj < 0) {
        max.wolves.level <- max.wolves.level + diff.adj - min.wolves.level
        min.wolves.level <- 0
    } else {
        min.wolves.level <- min.wolves.level - diff.adj
    }
    logbook("Adjusted wolf population for plotting steps [1, ", level.steps, "] to [",
        min.wolves.level, ", ", max.wolves.level, "] to make cells squarer")
} else if(diff.sheep.level < diff.wolf.level) {
    max.sheep.level <- max.sheep.level + diff.adj
    if(min.sheep.level - diff.adj < 0) {
        max.sheep.level <- max.sheep.level + diff.adj - min.sheep.level
        min.sheep.level <- 0
    } else {
        min.sheep.level <- min.sheep.level - diff.adj
    }
    logbook("Adjusted sheep population for plotting steps [1, ", level.steps, "] to [",
        min.sheep.level, ", ", max.sheep.level, "] to make cells squarer")
}

# Set up the grids for the level plots at each step
sheep.levels <- min.sheep.level:max.sheep.level
wolf.levels <- min.wolves.level:max.wolves.level
pop.levels <- expand.grid(sheep = sheep.levels, wolves = wolf.levels)
max.level <- 0
for(s in 1:level.steps) {
    step = paste0("step", s)
    pop.levels[, step] = rep(0, nrow(pop.levels))
}

# Second pass through the run files -- plot the population trajectory
# and gather data for the other plots

pdf(pdf.file)

plot.trajectory <- function(traj.files, traj.name, traj.maxima, level.maxima, level.stop,
    traj.stop = NULL, filter = NULL) {
    # Set up counters for different terminating conditions and populations
    # These only make sense if max.step is NULL
    extinctions <- list(`Both` = 0, `Wolves` = 0, `Sheep` = 0, `Neither` = 0)
    extinct.steps <- c(0)
    wolf.pops <- c(0)
    sheep.pops <- c(0)
    pop.levels <- expand.grid(sheep = level.maxima$sheep, wolves = level.maxima$wolves)
    max.level <- 0
    for(s in 1:level.stop) {
        step = paste0("step", s)
        pop.levels[, step] = rep(0, nrow(pop.levels))
    }

    if(log.xy) {
        plot(NULL, xlim = c(1, traj.maxima$sheep), ylim = c(1, traj.maxima$wolves), main = traj.name,
            xlab = "Sheep Population", ylab = "Wolf Population", log = "xy")
    } else {
        plot(NULL, xlim = c(0, traj.maxima$sheep), ylim = c(0, traj.maxima$wolves), main = traj.name,
            xlab = "Sheep Population", ylab = "Wolf Population")
    }

    n.traj <- length(traj.files)
    traj.all.dirs <- unlist(lapply(strsplit(traj.files, "/"), head, n = 1))
    traj.dirs <- unique(traj.all.dirs)
    clrs <- rainbow(n.traj, s = 0.8, v = 0.5, alpha = 0.2, start = 0, end = 1)
    pclr <- rainbow(n.traj, s = 0.8, v = 0.5, alpha = 0.8, start = 0, end = 1)
    dirpal <- list()
    if(length(traj.dirs) > 1) {
        rbow.frac <- length(traj.dirs) / (length(traj.dirs) + 1)
        clrs <- rainbow(length(traj.dirs), s = 0.8, v = 0.5, alpha = 0.2, start = 0, end = rbow.frac)
        pclr <- rainbow(length(traj.dirs), s = 0.8, v = 0.5, alpha = 0.8, start = 0, end = rbow.frac)
        for(i in 1:length(traj.dirs)) {
            dirpal[[traj.dirs[i]]] <- pclr[i]
        }
    } else {
        dirpal[[traj.dirs[1]]] = rgb(0.5, 0.5, 0.5, alpha = 0.8)
    }
    for(i in 1:n.traj) {
        file <- traj.files[i]

        # Get the colour for this line
        dir <- traj.all.dirs[i]
        ix <- i
        if(length(traj.dirs) > 1) {
            ix <- which(traj.dirs == dir)
        }
        l.clr <- clrs[ix]
        p.clr <- pclr[ix]


        # Read the data
        df <- read.csv(file, skip = 6, check.names = FALSE)

        # Remove data not wanted
        if(!is.null(traj.stop)) {
            df <- subset(df, `[step]` <= traj.stop)
        }
        if(!is.null(filter)) {
            for(field in names(filter)) {
                if(nrow(df) > 0) {
                    rows <- which(df[[field]] == filter[[field]])
                    df <- df[rows,]
                }
            }
        }

        n <- nrow(df)

        # Plot the trajectory
        if(n > 0) {
            end.pch <- 1
            if(df$`count sheep`[n] > 0 && df$`count wolves`[n] > 0) {
                end.pch <- 8
                extinctions$`Neither` <- extinctions$`Neither` + 1
            } else if(df$`count sheep`[n] > 0) {
                end.pch <- 3
                extinctions$`Wolves` <- extinctions$`Wolves` + 1
                extinct.steps <- c(extinct.steps, max(df$`[step]`))
                sheep.pops <- c(sheep.pops, df$`count sheep`[n])
            } else if(df$`count wolves`[n] > 0) {
                end.pch <- 4
                extinctions$`Sheep` <- extinctions$`Sheep` + 1
                extinct.steps <- c(extinct.steps, max(df$`[step]`))
                wolf.pops <- c(wolf.pops, df$`count wolves`[n])
            } else {
                extinctions$`Both` <- extinctions$`Both` + 1
                extinct.steps <- c(extinct.steps, max(df$`[step]`))
            }
            if(log.xy) {
                while((df$`count sheep`[n] == 0 || df$`count wolves`[n] == 0) && n > 0) {
                    n <- n - 1
                }
            }
            lines(c(df$`count sheep`[1:n]), c(df$`count wolves`[1:n]), col = l.clr)
            points(df$`count sheep`[1], df$`count wolves`[1], col = "black", pch = 16)
            if(plot.end) {
                points(df$`count sheep`[n], df$`count wolves`[n], col = p.clr, pch = end.pch)
                if(end.pch != 1) {
                    points(df$`count sheep`[n], df$`count wolves`[n], col = p.clr, pch = 0)
                }
            }
            
            for(s in 1:level.stop) {
                step <- paste0("step", s)
                step.row <- which(df$`[step]` == s)
                if(length(step.row) > 1) {
                    stop("BUG! Ambiguous row for step ", s)
                }
                n.sheep <- df$`count sheep`[step.row]
                n.wolves <- df$`count wolves`[step.row]
                r = which(pop.levels$sheep == n.sheep & pop.levels$wolves == n.wolves)
                if(length(r) > 1) {
                    stop("BUG! Ambiguous cell for ", n.sheep, " sheep and ", n.wolves, " wolves")
                }
                pop.levels[r, step] <- pop.levels[r, step] + 1
                if(pop.levels[r, step] > max.level) {
                    max.level <- pop.levels[r, step]
                }
            }
        }
    }
    return(list(`extinctions` = extinctions,
        `extinct.steps` = extinct.steps,
        `sheep.pops` = sheep.pops,
        `wolf.pops` = wolf.pops,
        `pop.levels` = pop.levels,
        `max.level` = max.level,
        `palette` = dirpal
    ))
}

plot.levels <- function(level.data, sheep.axis, wolf.axis, n.steps, level.max,
    grey.start = 0.3, grey.end = 0.9, grey.gamma = 2.2, grey.alpha = 1, rgb.others = 0) {
    n.plots <- length(level.data)
    if(n.plots > 3) {
        stop("Too many simultaneous level plots (", n.plots, ") -- maximum is 3", call. = FALSE)
    }

    grey.values <- rev(seq(grey.start^grey.gamma, grey.end^grey.gamma,
        length = level.max)^(1 / grey.gamma)) # From grey.colors() help in R

    fill.clr <- rep(NA, n.plots)
    for(i in 1:n.plots) {
        rgb.val <- c(rgb.others, rgb.others, rgb.others)
        rgb.val[i] <- grey.end
        if(i == n.plots && n.plots < 3) {
            for(k in (i + 1):3) {
                rgb.val[i] <- grey.end
            }
        }
        fill.clr[i] <- rgb(rgb.val[1], rgb.val[2], rgb.val[3], alpha = grey.alpha)
    }

    x.size <- 1 + sheep.axis[2] - sheep.axis[1]
    y.size <- 1 + wolf.axis[2] - wolf.axis[1]
    rgb.plot <- array(NA, dim = c(x.size, y.size, 3))

    for(s in 1:n.steps) {
        step <- paste0("step", s)

        plot(NULL, xlim = c(sheep.axis[1] - 0.5, sheep.axis[2] + 0.5),
            ylim = c(wolf.axis[1] - 0.5, wolf.axis[2] + 0.5),
            xlab = "Sheep Population", ylab = "Wolf Population",
            main = paste0("Step ", s))

        for(j in 1:n.plots) {
            pop <- level.data[[j]]$pop.levels
            for(i in 1:nrow(pop)) {
                n <- pop[i, step]
                if(n > level.max) {
                    n <- level.max
                }

                if(n > 0) {
                    x <- 1 + pop$sheep[i] - sheep.axis[1]
                    y <- 1 + pop$wolves[i] - wolf.axis[1]

                    if(x >= 1 && x <= x.size && y >= 1 && y <= y.size) {
                        rgb.plot[x, y, j] <- grey.values[n]
                        if(j == n.plots && n.plots < 3) {
                            for(k in (j + 1):3) {
                                rgb.plot[x, y, k] <- grey.values[n]
                            }                        
                        }
                    }
                }
            }
        }

        for(x in sheep.axis[1]:sheep.axis[2]) {
            for(y in wolf.axis[1]:wolf.axis[2]) {
                r <- rgb.plot[1 + x - sheep.axis[1], 1 + y - wolf.axis[1], 1]
                g <- rgb.plot[1 + x - sheep.axis[1], 1 + y - wolf.axis[1], 2]
                b <- rgb.plot[1 + x - sheep.axis[1], 1 + y - wolf.axis[1], 3]
                if(!is.na(r) || !is.na(g) || !is.na(b)) {
                    if(is.na(r)) r <- rgb.others
                    if(is.na(g)) g <- rgb.others
                    if(is.na(b)) b <- rgb.others
                    clr <- rgb(r, g, b, alpha = grey.alpha)
                    rect(x - 0.5, y - 0.5, x + 0.5, y + 0.5, col = clr, border = NA)
                }
            }
        }

        if(n.plots > 1) {
            legend("top", legend = names(level.data), fill = fill.clr, bg = "white",
                border = fill.clr)
        }

    }
}

plot.summary <- function(cmp.list, palette = unlist(cmp.list[[1]]$palette), logxy = TRUE) {

    # Extinctions barplot and preparations needed to get axes limits
    extinctions <- matrix(nrow = length(cmp.list), ncol = length(cmp.list[[1]]$extinctions))
    max.extinct.step <- 0
    max.extinct.count <- 0

    min.sheep.pop <- NA
    max.sheep.pop <- 0
    max.sheep.count <- 0

    min.wolf.pop <- NA
    max.wolf.pop <- 0
    max.wolf.count <- 0

    for(i in 1:length(cmp.list)) {
        for(j in 1:length(cmp.list[[i]]$extinctions)) {
            extinctions[i, j] <- cmp.list[[i]]$extinctions[[j]]
            if(names(cmp.list[[i]]$extinctions)[j] != names(cmp.list[[1]]$extinctions[j])) {
                stop("BUG! (mismatched extinction names)")
            }
        }

        ES <- cmp.list[[i]]$extinct.steps
        SP <- cmp.list[[i]]$sheep.pops
        WP <- cmp.list[[i]]$wolf.pops
        if(logxy) {
            SP <- log10(SP[which(SP > 0)])
            WP <- log10(WP[which(WP > 0)])
        }

        max.ES <- max(ES)
        hist.ES <- hist(ES, plot = FALSE)
        count.ES <- max(hist.ES$counts)

        hist.SP <- hist(SP, plot = FALSE)
        min.SP <- min(hist.SP$breaks)
        max.SP <- max(hist.SP$breaks)
        count.SP <- max(hist.SP$counts)

        hist.WP <- hist(WP, plot = FALSE)
        min.WP <- min(hist.WP$breaks)
        max.WP <- max(hist.WP$breaks)
        count.WP <- max(hist.WP$counts)

        if(i == 1) {
            max.extinct.step <- max.ES
            max.extinct.count <- count.ES

            min.sheep.pop <- min.SP
            max.sheep.pop <- max.SP
            max.sheep.count <- count.SP

            min.wolf.pop <- min.WP
            max.wolf.pop <- max.WP
            max.wolf.count <- count.WP
        } else {
            if(max.ES > max.extinct.step) max.extinct.step <- max.ES
            if(count.ES > max.extinct.count) max.extinct.count <- count.ES

            if(min.SP < min.sheep.pop) min.sheep.pop <- min.SP
            if(max.SP > max.sheep.pop) max.sheep.pop <- max.SP
            if(count.SP > max.sheep.count) max.sheep.count <- count.SP

            if(min.WP < min.wolf.pop) min.wolf.pop <- min.WP
            if(max.WP > max.wolf.pop) max.wolf.pop <- max.WP
            if(count.WP > max.wolf.count) max.wolf.count <- count.WP
        }
    }
    barplot(extinctions, names.arg = names(cmp.list[[1]]$extinctions),
        main = NULL, ylab = "Numbers of runs", xlab = "Extinctions",
        beside = (length(cmp.list) > 1), col = palette,
        legend.text = names(cmp.list))

    # Extinction step histogram
    for(i in 1:length(cmp.list)) {
        add.plot <- (i > 1)
        hist(cmp.list[[i]]$extinct.steps, ylab = "Numbers of runs", xlab = "Extinction Step",
            add = add.plot, col = palette[i], border = FALSE, xlim = c(0, max.extinct.step),
            ylim = c(0, max.extinct.count), main = "")
    }
    legend("topright", legend = names(cmp.list), fill = palette, border = palette,
        bg = "white")

    # Wolf/Sheep populations at extinction histograms
    if(logxy) {
        for(i in 1:length(cmp.list)) {
            add.plot <- (i > 1)
            hist(log10(cmp.list[[i]]$sheep.pops), ylab = "Numbers of runs",
                add = add.plot, col = palette[i], border = FALSE, main = "",
                xlim = c(min.sheep.pop, max.sheep.pop), ylim = c(0, max.sheep.count),
                xlab = "Log(Sheep Population) at Wolf Extinction")
        }
        legend("topright", legend = names(cmp.list), fill = palette, border = palette,
            bg = "white")
        for(i in 1:length(cmp.list)) {
            add.plot <- (i > 1)
            hist(log10(cmp.list[[i]]$wolf.pops), ylab = "Numbers of runs",
                add = add.plot, col = palette[i], border = FALSE, main = "",
                xlim = c(min.wolf.pop, max.wolf.pop), ylim = c(0, max.wolf.count),
                xlab = "Log(Wolf Population) at Sheep Extinction")
        }
        legend("topright", legend = names(cmp.list), fill = palette, border = palette,
            bg = "white")
    } else {
        for(i in 1:length(cmp.list)) {
            add.plot <- (i > 1)
            hist(cmp.list[[i]]$sheep.pops, ylab = "Numbers of runs",
                add = add.plot, col = palette[i], border = FALSE, main = "",
                xlim = c(min.sheep.pop, max.sheep.pop), ylim = c(0, max.sheep.count),
                xlab = "Sheep Population at Wolf Extinction")
        }
        legend("topright", legend = names(cmp.list), fill = palette, border = palette,
            bg = "white")
        for(i in 1:length(cmp.list)) {
            add.plot <- (i > 1)
            hist(cmp.list[[i]]$wolf.pops, ylab = "Numbers of runs",
                add = add.plot, col = palette[i], border = FALSE, main = "",
                xlim = c(min.wolf.pop, max.wolf.pop), ylim = c(0, max.wolf.count),
                xlab = "Wolf Population at Sheep Extinction")
        }
        legend("topright", legend = names(cmp.list), fill = palette, border = palette,
            bg = "white")
    }
}

run.data <- plot.trajectory(run.files, expt.name, list(`sheep` = max.sheep, `wolves` = max.wolves),
    list(`sheep` = sheep.levels, `wolves` = wolf.levels), level.steps, traj.stop = max.step)

logbook("Trajectory plot complete for run data")

run.list <- list()
run.list[[expt.name]] <- run.data
plot.levels(run.list, c(min.sheep.level, max.sheep.level),
    c(min.wolves.level, max.wolves.level), level.steps, run.data$max.level)

logbook("Levelplots complete for run data")

if(is.null(max.step)) {
    plot.summary(run.list, logxy = log.xy)
    logbook("Summary plots complete for run data")
}

if(length(cmp.files) > 0) {
    for(i in 1:length(cmp.files)) {
        cmp.name <- names(cmp.files)[i]
        cmp.data <- plot.trajectory(cmp.files[[i]], cmp.name,
        list(`sheep` = max.sheep, `wolves` = max.wolves),
        list(`sheep` = sheep.levels, `wolves` = wolf.levels), level.steps, traj.stop = max.step)
        run.list[[cmp.name]] <- cmp.data
    }
    all.data <- plot.trajectory(all.files, "Trajectory Comparison",
        list(`sheep` = max.sheep, `wolves` = max.wolves),
        list(`sheep` = sheep.levels, `wolves` = wolf.levels), level.steps, traj.stop = max.step)
    plot.levels(run.list, c(min.sheep.level, max.sheep.level),
        c(min.wolves.level, max.wolves.level), level.steps, run.data$max.level)
    plot.summary(run.list, logxy = log.xy, palette = unlist(all.data$palette))
}

dev.off()
q(status = 0)