################################################################################
# One SLURM array task: run this chunk's replications and save chunk_%04d.rds.
# Env: TIER_B_N (sample size), TIER_B_N_CHUNKS, TIER_B_N_REPLICATIONS,
#      TIER_B_METHOD ("nominal" | "narrow"), TIER_B_OUTDIR.
# chunk_id auto-detected from SLURM_ARRAY_TASK_ID by medsim_config().
################################################################################

source("tier_b_synthetic.R")

geti <- function(k, d) as.integer(Sys.getenv(k, unset = as.character(d)))
N            <- geti("TIER_B_N", 200L)
n_chunks     <- geti("TIER_B_N_CHUNKS", 4L)
n_rep        <- geti("TIER_B_N_REPLICATIONS", 200L)
method_name  <- Sys.getenv("TIER_B_METHOD", unset = "nominal")
outdir       <- Sys.getenv("TIER_B_OUTDIR", unset = "tier_b_results")

method <- switch(method_name,
                 nominal = tier_b_method_nominal,
                 narrow  = tier_b_method_narrow,
                 stop("TIER_B_METHOD must be 'nominal' or 'narrow'"))

config <- medsim_config(mode = "cluster", n_chunks = n_chunks,
                        mem_per_cpu = "512M", output_dir = outdir)
config$n              <- N
config$n_replications <- n_rep

medsim_run_chunk(build_tier_b_scenarios(), method, config, verbose = TRUE)
