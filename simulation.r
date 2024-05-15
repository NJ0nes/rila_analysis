# ========= PARAMETERS

drift = 0.0966
volatility = 0.1892
n_simulations = 100000

stepcount = 6 * 252
dt = 1 / 252


# ========= CREDITED RATES

# ==== Index Perfomance
func_indexperf_6_10 = function(ret) {
    if (ret > 0) {
        1.3 * ret
    }
    else {
        min(ret + 0.10, 0)
    }
}

func_indexperf_6_20 = function(ret) {
    if (ret > 0) {
        1.2 * ret
    }
    else {
        min(ret + 0.2, 0)
    }
}

func_indexperf_3_10 = function(ret) {
    if (ret > 0) {
        1.1 * ret
    }
    else {
        min(ret + 0.1, 0)
    }
}

func_indexperf_3_20 = function(ret) {
    if (ret > 0) {
        ret
    }
    else {
        min(ret + 0.2, 0)
    }
}

func_indexperf_1_10 = function(ret) {
    if (ret > 0) {
        min(ret, 0.2125)
    }
    else {
        min(ret + 0.1, 0)
    }
}

func_indexperf_1_20 = function(ret) {
    if (ret > 0) {
        min(ret, 0.14)
    }
    else {
        min(ret + 0.2, 0)
    }
}

func_indexperf_1_30 = function(ret) {
    if (ret > 0) {
        min(ret, 0.1150)
    }
    else {
        min(ret + 0.3, 0)
    }
}

# ==== Other 1 year contracts

func_index_precision = function(ret) {
    if (ret > 0) {
        0.1140
    }
    else {
        min(ret + 0.10, 0)
    }
}

func_index_dual_precision = function(ret) {
    if (ret > -0.1) {
        0.0990
    }
    else {
        ret + 0.1
    }
}

func_index_guard = function(ret) {
    if (ret > 0) {
        min(ret, 0.18)
    }
    else {
        max(ret, -0.1)
    }
}

func_index_protection = function(ret) {
    if (ret > 0) {
        0.052
    }
    else {
        0.0
    }
}

# ========= SIMULATION CODE

relchange = function(a, b) {
    (b - a) / a
}

compounded_return = function(paths, sim, termlength, func) {
    out = 1

    for (i in 1:(6 / termlength)) {
        out = out * (1 + func(relchange(paths[(i - 1) * termlength * 252 + 1, sim],
                                    paths[i * termlength * 252 + 1, sim])))
    }

    out - 1
}

set.seed(123)

# Need the + 1 on nrow because we need to include the starting value
all_paths = matrix(nrow = stepcount + 1, ncol = n_simulations)
all_paths[1, ] = 1 

for (s in 1:n_simulations) {
    # Euler-Maryuama method
    for (i in 2:(stepcount + 1)) {
        all_paths[i, s] = all_paths[i - 1, s] * (1 + drift * dt + volatility *
                 rnorm(n = 1, mean = 0, sd = sqrt(dt)))
    }
}

final_rates = data.frame(simulation = 1:n_simulations)
final_rates[, "raw"] = NA
final_rates[, "indexperf_6_10"] = NA
final_rates[, "indexperf_6_20"] = NA
final_rates[, "indexperf_3_10"] = NA
final_rates[, "indexperf_3_20"] = NA
final_rates[, "indexperf_1_10"] = NA
final_rates[, "indexperf_1_20"] = NA
final_rates[, "indexperf_1_30"] = NA
final_rates[, "index_precision"] = NA
final_rates[, "index_dual_precision"] = NA
final_rates[, "index_guard"] = NA
final_rates[, "index_protection"] = NA

for (s in 1:n_simulations) {
    final_rates[s, "raw"] = relchange(all_paths[1, s], all_paths[stepcount + 1, s])
    final_rates[s, "indexperf_6_10"] = compounded_return(all_paths, s, 6, func_indexperf_6_10)
    final_rates[s, "indexperf_6_20"] = compounded_return(all_paths, s, 6, func_indexperf_6_20)
    final_rates[s, "indexperf_3_10"] = compounded_return(all_paths, s, 3, func_indexperf_3_10)
    final_rates[s, "indexperf_3_20"] = compounded_return(all_paths, s, 3, func_indexperf_3_20)
    final_rates[s, "indexperf_1_10"] = compounded_return(all_paths, s, 1, func_indexperf_1_10)
    final_rates[s, "indexperf_1_20"] = compounded_return(all_paths, s, 1, func_indexperf_1_20)
    final_rates[s, "indexperf_1_30"] = compounded_return(all_paths, s, 1, func_indexperf_1_30)
    final_rates[s, "index_precision"] = compounded_return(all_paths, s, 1, func_index_precision)
    final_rates[s, "index_dual_precision"] = compounded_return(all_paths, s, 1, func_index_dual_precision)
    final_rates[s, "index_guard"] = compounded_return(all_paths, s, 1, func_index_guard)
    final_rates[s, "index_protection"] = compounded_return(all_paths, s, 1, func_index_protection)
}

saveRDS(final_rates, file="paths.rda")
