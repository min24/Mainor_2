#http://wardoctor.nosoc.io/public/paulokopny/vkauth/karepin.html
get_friends_graph<-function(code, debug=F){
  
  #  library(httr)
  #  library(stringr) 
  #  library(devtools)
  #  code = "43c15396a3efce267e"
  
  # Загружаем функции для работы с VK API
  source_url("https://gist.githubusercontent.com/paulokopny/63daf8ca42f9d842b122/raw/bf7c8f9f6944b44e7c791cb66f4919bd762f4dc9/vk.R")
  
  vk = get.vk.connector(code = code, app = "karepin", debug = debug)

  friends.list = vk(method = "friends.get", v = "5.73")
  friends.list = friends.list$items
  
  rest = length(friends.list)
  
  friends.info <- list()
  bunch_size <- 300
  
  while(rest > bunch_size){
    id.list = create_user_ids(friends.list[(length(friends.list)-rest+1):(length(friends.list)-rest+bunch_size)])
    friends.info.sub <- vk(method = "users.get", user_ids = id.list, v = "5.73")
    friends.info <- rbind(friends.info, friends.info.sub)
    rest = rest - bunch_size
    Sys.sleep(.4)
  }
  
  id.list <- create_user_ids(friends.list[(length(friends.list)-rest+1):length(friends.list)])
  friends.info.sub <- vk(method = "users.get", user_ids = id.list, v = "5.73")
  friends.info <- c(friends.info, friends.info.sub)

  Edge.list = c();
  pb<-txtProgressBar(min = 0, max = length(friends.list), initial = 0, char = "=",
                 width = NA, title, label, style = 3, file = "")
  cntr <- 0; 
  
  for(i in friends.list){
    mutual.list <- vk(method="friends.getMutual", target_uid = i, v = "5.73")
    Sys.sleep(.5)
      for (j in mutual.list){
        Edge.list = rbind(Edge.list,c(i, j))
      }
    cntr <- cntr + 1
    setTxtProgressBar(pb, cntr)
  }
  
  info = plyr::rbind.fill(lapply(friends.info, function(x){as.data.frame(t(x))}))
  info$fullname = str_c(info$first_name, " ", info$last_name)
  return(list(el=Edge.list, info=info))
}
