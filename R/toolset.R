read_img <-
  function(path){
    imager::load.image(path)[,,1,1]
  }

calc_ndi <-
  function(path, band1 = 660, band2 = 750){
    bmp_files <- dir(path, full.names = TRUE, recursive = TRUE, pattern = "bmp")

    img1 <-
      bmp_files %>%
      stringr::str_subset(paste0(band1, "nm")) %>%
      read_img

    img2 <-
      bmp_files %>%
      stringr::str_subset(paste0(band2, "nm")) %>%
      read_img

    img <- (img1 - img2) / (img1 + img2)

    return(img)
  }

visualize <-
  function(img){
    img %>%
      tibble::as_tibble %>%
      tibble::rowid_to_column("col") %>%
      gather(row, value, -col) %>%
      dplyr::mutate(row = readr::parse_number(row)) %>%
      ggplot2::ggplot(ggplot2::aes(row, col, fill = value)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_viridis()
  }



