
# install.packages("KeyboardSimulator")
# install.packages("tesseract")
# install.packages("magick")
# install.packages("stringr")
library("KeyboardSimulator")
library(magrittr)
library(data.table)
library(tesseract)
library(magick)
library(stringr)

is_odd <- function(x) {(x %% 2) == 1}
is_even <- function(x) {(x %% 2) == 0}


eng <- tesseract("eng")
setwd("C:\\Program Files (x86)\\Auto Screen Capture")
screenshot_dir <- "B:\\Games\\ROK_Screenshots"

random_delay <- function(min = .255, max = 0.551) {
  y <- runif(1, min = min, max = max)
  Sys.sleep(.25+y)
}

m_clicker <- function() {
  KeyboardSimulator::mouse.click("left", hold = TRUE)
  random_delay()
  KeyboardSimulator::mouse.release(button = "left")
}
# m_clicker()



# click on profile picture
KeyboardSimulator::mouse.move(215, 160, duration = 0.78)
y <- runif(1, min = -.35, max = 0.42)
random_delay()
m_clicker()
random_delay()

# go to rankings and click on that
KeyboardSimulator::mouse.move(678, 742, duration = 0.78)
random_delay()
m_clicker()

# go to individual power
KeyboardSimulator::mouse.move(544, 584, duration = 0.78)
random_delay()
m_clicker()
random_delay()

count_screenshot <- function () {
  list.files(path = screenshot_dir, pattern = ".png") %>% length
}

scan_one_person <- function(x = 714, y = 385) {
  # first person
  system("autoscreen -capture") # initial screenshot
  initial_screenshot <- count_screenshot()

  KeyboardSimulator::mouse.move(x, y, duration = 0.78)
  random_delay()
  m_clicker()
  random_delay()
  system("autoscreen -capture")

  if (count_screenshot() -initial_screenshot > 0) {
    # kill statistics
    KeyboardSimulator::mouse.move(1191, 442, duration = 0.78)
    random_delay()
    m_clicker()
    random_delay()
    system("autoscreen -capture")
    # find 'more info'
    KeyboardSimulator::mouse.move(523, 727, duration = 0.78)
    random_delay()
    m_clicker()
    random_delay()
    system("autoscreen -capture")
    # press escape
    KeyboardSimulator::mouse.move(1445, 169, duration = 0.78)
    random_delay()
    m_clicker()
    random_delay()
    # press escape again
    KeyboardSimulator::mouse.move(1418, 213, duration = 0.78)
    random_delay()
    m_clicker()
    random_delay()
  } else {
    break
    # move to the next person in line
  }
} # end of scan_one_person

scan_one_person(x = 717, y = 378)

all_pics <- list.files(screenshot_dir, full.names = TRUE, pattern = ".png")
all_pics_DT <- data.table(
  file_name = all_pics,
  create_date = file.mtime(all_pics))
setorder(all_pics_DT, -create_date)


power_ranking_txt <- image_read(all_pics_DT[4,1][[1]]) %>%
  magick::image_resize("1000") %>%
  magick::image_crop(
    geometry_area(width = 1000, height = 500, x_off = 130, y_off = 130)) %>%
  magick::image_ocr() %>% data.table::tstrsplit(., split = "\n\n|\n") %>%
  unlist %>% gsub(pattern = "“|»|,", replace = "", x = .)


grep(power_ranking_txt,
     pattern = ".*[0-9]{7,}$", value = TRUE) %>%
  strsplit(., split = "[0-9]{7,}$")


data.table(player_row = grep(power_ranking_txt,
  pattern = ".*[0-9]{7,}$", value = TRUE)) %>%
  .[, player_names := strsplit(player_row, split = "[0-9]{7,}$")] %>%
  .[, player_power := stringr::str_extract(player_row, pattern = " [0-9]{7,}")] %>%
  .[]

## Idea #1 - grab non Alliance text...
# if (is_even(length(power_ranking_txt))) {
#   prt_lab <- rep(c("Player", "Alliance"), length(power_ranking_txt)/2)
#   names(power_ranking_txt) <- prt_lab
#   ifelse(names(power_ranking_txt) == "Player", power_ranking_txt, NA)
#   Filter(power_ranking_txt, f = function(x) names(x) == "Player")
# }







move_up_one_rank <- function() {
  KeyboardSimulator::mouse.move(x = 714, y = 375, duration = 0.55)
  KeyboardSimulator::mouse.click(button = "left", hold = TRUE)
  KeyboardSimulator::mouse.move(x= 714, y = 295, duration = 2)
  KeyboardSimulator::mouse.release(button = "left")
  random_delay(min = 0.8, max = 1.1)
}


# Sys.sleep(3)
# KeyboardSimulator::keybd.press("esc")
