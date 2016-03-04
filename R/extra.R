# library(dplyr)
# library(tidyr)
# library(purrr)
# library(WRTDStidal)
# library(foreach)
# library(doParallel)
# # devtools::load_all('M:/docs/wtreg_for_estuaries')
# 
# cl <- makeCluster(6)
# registerDoParallel(cl)
# 
# data(tomod)
# 
# lablk <- list(
#   shrt = c('din', 'nh', 'no23'),
#   lngs = c(
#     expression(paste('ln-dissolved inorganic nitrogen (mg ', L^-1, ')')),
#     expression(paste('ln-ammonium (mg ', L^-1, ')')),
#     expression(paste('ln-nitrite/nitrate (mg ', L^-1, ')'))
#     )
#   )
# 
# # weight reg by group
# mods_opt <- mutate(tomod, resdup = resvar) %>% 
#   group_by(Location, Site_Code, resvar, flovar) %>% 
#   nest %>% 
#   mutate(mod = map(
#     data, function(x){ 
#   
#     # get label from resdup column
#     reslab <- with(lablk, lngs[shrt == unique(x$resdup)])
#     
#     # create tidal object to model
#     tmp <- select(x, -resdup) %>% 
#       rename(
#         res = resval, 
#         flo = flolag
#       ) %>% 
#       data.frame %>% 
#       tidal(., 
#         reslab = reslab, 
#         flolab = expression(paste('ln-flow (standardized)'))
#       )
#     
#     # create model and exit
#     out <- winsrch_optim(tmp, tau = 0.5, min_obs = NULL, lower = c(0.1, 1, 0.1), upper = c(2, 15, 2),
#       control = list(factr = 1e12, parscale = c(1.9, 14, 1.9))
#     )
# 
#     # create mod with optimum 
#     mod <- modfit(tmp, tau = c(0.1, 0.5, 0.9), min_obs = NULL, wins = as.list(out$par), flo_div = 30)
#   
#     return(mod)
#   
#   }))
# 
# save(mods_opt, file = 'data/mods_opt.RData', compress = 'xz')

flo <- mods$mod[[1]]
florng <- attr(flo, 'floobs_rng')
flolab <- attr(flo, 'flolab')
flo <- dplyr::select(flo, date, flo) %>% 
  mutate(flo = flo * abs(diff(florng)) + florng[1])

yrrng <- c(1990, 2000)
yrrng <- as.Date(paste0(yrrng, '-01-01'))
p1 <- ggplot(flo, aes(x = date, y = flo)) + 
  geom_line() +
  scale_y_continuous('ln - flow') +
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  scale_x_date(limits = yrrng)
p2 <- fitplot(mods$mod[[1]], annuals = F, predicted = T, alpha = 0.7) +
  scale_x_date(limits = yrrng)
p3 <- fitplot(mods$mod[[1]], annuals = F, predicted = F, alpha = 0.7) +
  scale_x_date(limits = yrrng)

