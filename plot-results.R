#!/usr/bin/env Rscript

# Defaults

n.runs <- NULL # The default is to plot all the runs
log.xy <- FALSE # Don't plot log axes
max.step <- NULL # The default is to plot all steps
plot.end <- TRUE # Plot end-points
level.steps <- 5 # How many steps to plot using 'levelplot'

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
    cat("Usage: ./plot-results.R [--runs <runlist>] [--max-step <t>] [--log-xy] <dir> [PDF file]\n")
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
    } else {
        stop("Option ", opt, " not recognized. Try --help for usage.", call = FALSE)
    }
}

if(length(args) < 1) {
    stop("No directory specified", call = FALSE)
}

dir <- args[1]

if(!file.exists(dir)) {
    stop("Directory ", dir, " does not exist", call = FALSE)
}

if(!file.info(dir)$isdir) {
    stop("Directory argument ", dir, " exists, but is not a directory", call = FALSE)
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
        stop("No NetLogo run files found in directory ", dir, call = FALSE)
    }
    n.runs = length(run.files)
} else {
    for(run.id in run.list) {
        pat <- paste0("*-", run.id, "-table.csv")
        mat <- list.files(path = dir, pattern = pat, full.names = TRUE)
        if(length(mat) == 1) {
            run.files <= c(run.files, mat)
        } else {
            stop("Zero or non-unique match for ", pat, " in ", dir, call = FALSE)
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
logbook("Sheep population in steps [1, ", level.steps, "] is in range [", min.sheep.level, ", ", max.sheep.level, "]")
logbook("Wolf population in steps [1, ", level.steps, "] is in range [", min.wolves.level, ", ", max.wolves.level, "]")
sheep.levels <- min.sheep.level:max.sheep.level
wolf.levels <- min.wolves.level:max.wolves.level
pop.levels <- expand.grid(sheep = sheep.levels, wolves = wolf.levels)
max.level <- 0
for(s in 1:level.steps) {
    step = paste0("step", s)
    pop.levels[, step] = rep(0, nrow(pop.levels))
}

# Second pass through the run files -- plot the population trajectory

pdf(pdf.file)

if(log.xy) {
    plot(NULL, xlim = c(1, max.sheep), ylim = c(1, max.wolves),
        xlab = "Sheep Population", ylab = "Wolf Population", log = "xy")
} else {
    plot(NULL, xlim = c(0, max.sheep), ylim = c(0, max.wolves),
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
    } else if(df$`count sheep`[n] > 0) {
        end.pch <- 3
    } else if(df$`count wolves`[n] > 0) {
        end.pch <- 4
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
    logbook("Started levelplot at time step, ", s)

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

dev.off()
q(status = 0)