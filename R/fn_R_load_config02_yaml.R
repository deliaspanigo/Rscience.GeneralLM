#' @export
fn_R_load_config02_yaml <- function(){
folder_path <- fn_PK_yaml_config02_tools_folder_path()
# print(folder_path)
all_files <- list.files(path = folder_path, pattern = "\\.yaml$",
                        all.files = T, full.names = T)
names(all_files) <-  tools::file_path_sans_ext(basename(all_files))
# print(all_files)

list_all_config02_tools <- sapply(all_files, yaml::yaml.load_file, USE.NAMES = T)
new_names <- names(list_all_config02_tools)
new_names <- gsub(pattern = ".config", replacement = "", x = new_names)
names(list_all_config02_tools) <- new_names

list_all_config02_tools
}
