library(tidyr)
library(dplyr)
library(repurrrsive)

users <- tibble(user = gh_users)

users %>% unnest_wider(user)

users %>% hoist(user, 
                followers = "followers", 
                login = "login", 
                url = "html_url"
)

repos <- tibble(repo = gh_repos)
repos

repos <- repos %>% unnest_longer(repo)
repos

repos %>% hoist(repo, 
                login = c("owner", "login"), 
                name = "name",
                homepage = "homepage",
                watchers = "watchers_count"
)
