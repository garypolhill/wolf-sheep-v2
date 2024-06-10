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
    } else {
        stop("Option ", opt, " not recognized. Try --help for usage.", call. = FALSE)
    }
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

# First pass through the run files -- find the maximum sheep and wolf populations

max.sheep <- 0
max.sheep.level <- 0
max.wolves <- 0
max.wolves.level <- 0
min.sheep <- NULL
min.sheep.level <- NULL
min.wolves <- NULL
min.wolves.level <- NULL

for(file in run.files) {
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

# Set up counters for different terminating conditions and populations
# These only make sense if max.step is NULL
extinctions <- list(`Both` = 0, `Wolves` = 0, `Sheep` = 0, `Neither` = 0)
extinct.steps <- c(0)
wolf.pops <- c(0)
sheep.pops <- c(0)

# Second pass through the run files -- plot the population trajectory
# and gather data for the other plots

pdf(pdf.file)

if(log.xy) {
    plot(NULL, xlim = c(1, max.sheep), ylim = c(1, max.wolves), main = expt.name,
        xlab = "Sheep Population", ylab = "Wolf Population", log = "xy")
} else {
    plot(NULL, xlim = c(0, max.sheep), ylim = c(0, max.wolves), main = expt.name,
        xlab = "Sheep Population", ylab = "Wolf Population")
}

clrs <- rainbow(n.runs, s = 0.8, v = 0.5, alpha = 0.2, start = 0, end = 1)
pclr <- rainbow(n.runs, s = 0.8, v = 0.5, alpha = 0.8, start = 0, end = 1)
for(i in 1:length(run.files)) {
    file <- run.files[i]
    l.clr <- clrs[i]
    p.clr <- pclr[i]
    df <- read.csv(file, skip = 6, check.names = FALSE)
    if(!is.null(max.step)) {
        df <- subset(df, `[step]` <= max.step)
    }

    n <- nrow(df)

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
    
    for(s in 1:level.steps) {
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
logbook("Population trajectory plot complete")

greys <- rev(grey.colors(max.level))
for(s in 1:level.steps) {
    step <- paste0("step", s)

    plot(NULL, xlim = c(min.sheep.level - 0.5, max.sheep.level + 0.5),
        ylim = c(min.wolves.level - 0.5, max.wolves.level + 0.5),
        xlab = "Sheep Population", ylab = "Wolf Population",
        main = paste0("Step ", s))

    for(i in 1:nrow(pop.levels)) {
        n <- pop.levels[i, step]
        if(n > 0) {
            x <- pop.levels$sheep[i]
            y <- pop.levels$wolves[i]

            rect(x - 0.5, y - 0.5, x + 0.5, y + 0.5, col = greys[n], border = NA)
        }
    }
    logbook("Population levelplot at time step ", s, " complete")
}

if(is.null(max.step)) {
    barplot(unlist(extinctions), names.arg = names(extinctions),
        main = expt.name, ylab = "Numbers of runs", xlab = "Extinctions")
    hist(extinct.steps, ylab = "Numbers of runs", xlab = "Extinction Step",
        main = expt.name)
    if(log.xy) {
        hist(log10(sheep.pops), ylab = "Numbers of runs", xlab = "Log(Sheep Population) at Wolf Extinction",
            main = expt.name)
        hist(log10(wolf.pops), ylab = "Numbers of runs", xlab = "Log(Wolf Population) at Sheep Extinction",
            main = expt.name)
    } else {
        hist(sheep.pops, ylab = "Numbers of runs", xlab = "Sheep Population at Wolf Extinction",
            main = expt.name)
        hist(wolf.pops, ylab = "Numbers of runs", xlab = "Wolf Population at Sheep Extinction",
            main = expt.name)
    }
}

dev.off()
q(status = 0)