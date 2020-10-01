#' Score CREDI response data
#'
#' This calculates the posterior density function.
#' @param data (data.frame) Defaults to NULL. Response data. If NULL, then user is prompted to identify a .csv file with response data. Defaults to NULL.
#' @param reverse_code (Logical) Defaults to TRUE. If TRUE, then reverse coding is automated to appropriately handle the negatively worded items LF9, LF102, LFMH1, LFMH2, LFMH3, LFMH4, LFMH5, LFMH7, LFMH8, & LFMH9. If FALSE, then no reverse coding is applied.
#' @param interactive (Logical) Defaults to TRUE. If TRUE, the user may be prompted with caution messages regarding whether scoring should be continued, where to save the scores, where to save a logfile, etc. If FALSE, continuation is assumed and scores and the user is not prompted to save scores or a logfile.
#' @param min_items (integer) Default to 5. The minimum number of scale-specific items (e.g., SF, MOT, etc.) required for a score to be calculated.
#' @keywords CREDI
#' @importFrom magrittr %>%
#' @importFrom readr read_csv
#' @importFrom readr write_csv
#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr n_distinct
#' @importFrom tibble rownames_to_column
#' @export
#' @examples
#' score()

# reverse_code = FALSE
# save_logfile = TRUE
# interactive = FALSE
# data = input_df

score<-function(data = NULL, reverse_code = TRUE, interactive = TRUE, min_items = 5){

  # Identify if dialog specifying .csv file should be bypassed.

  bypass = ifelse(is.null(data), FALSE, TRUE)
  if (bypass == TRUE){
    if (!is.data.frame(data)){
      stop("data argument must be type data.frame")
    }
  }

  # Load required packages

  require("stats")
  require("svDialogs")
  require("tidyverse")

  # Created log file
  time1 = proc.time()
  log = list(c("------------------------------------"), c("Log for CREDI Scoring Messages"),
             paste("Date:", Sys.time()), c("------------------------------------"))


  # Load in the response data, if not bypassed
  csv_wd = getwd()
  if(bypass == FALSE){
    out_dlgOpen = dlgOpen(title = "Select the .csv file with the CREDI response data",
                          filters = c("csv", "*.csv"))
    csv_file = out_dlgOpen$res
    if (!endsWith(tolower(csv_file), ".csv")){stop("Selected file is not a .csv file.", call. = FALSE)}
    csv_wd = paste(strsplit(csv_file,"/")[[1]][-length(strsplit(csv_file,"/")[[1]])],collapse = "/")
    setwd(csv_wd)
    input_df = readr::read_csv(file = csv_file, col_types = readr::cols())
  } else {
    input_df = data
  }

  # Clean the input data
  list_cleaned = clean(input_df = input_df, mest_df = mest_df, reverse_code = reverse_code,
                       interactive = interactive, log = log)
  log = list_cleaned$log
  if(list_cleaned$stop!=0){

    #print("*Error: Processing the provided response data resulted in errors. See log for more details.")
    return(list(log = log))

    if (interactive == TRUE){
      x<-as.character(readline(prompt = "Would you like to save a log file of warning and error messages? [Y/N]: "))
      x <- toupper(x)
      cut = 0
      while(cut == 0){
        if (x == "Y"){
          cut = 1;
        } else if (x == "N"){
          cut = 1
          stop("Scoring canceled.", call. = FALSE)
        } else {
          x<-as.character(readline(prompt = "Would you like to continue? [Y/N]:"))
          x <- toupper(x)
          cut = 0
        }
      } #end while
      write_log(log = log, folder = csv_wd)
    } #End if interactive

    #return(list(log = log))

  } # End if stop != 0
  cleaned_df = list_cleaned$cleaned_df
  sf_df = list_cleaned$sf_df
  items_noresponse = list_cleaned$items_noresponse


  # Crate data matricies
  X = model.matrix(~1 + I( (AGE-18)/10.39 ) + I( ((AGE-18)/10.39)^2 ) + I( ((AGE-18)/10.39)^3 ), data = cleaned_df)
  X_4 = model.matrix(~1 + I( (AGE-18)/10.39 ) + I( ((AGE-18)/10.39)^2 ) + I( ((AGE-18)/10.39)^3 ) + I( ((AGE-18)/10.39)^4 ), data = cleaned_df)
  Y = as.matrix(cleaned_df[,-match(c("ID","AGE",items_noresponse), names(cleaned_df))]); Y[is.na(Y)] = -9L
  Y_sf = as.matrix(sf_df[,-na.omit(match(c("ID","AGE","age_group",items_noresponse), names(sf_df)))]); Y_sf[is.na(Y_sf)] = -9L
  MU_LF = X%*%as.matrix(B) #NxK (matrix)
  MU_SF = X%*%as.numeric(beta) #Nx1

  # Obtain necessary parameter matricies
  inds_exclude = match(items_noresponse, mest_df$CREDI_code)
  if (length(inds_exclude)==0){
    LAMBDA = as.matrix(mest_df[,c("MOT","COG","LANG","SEM")])
    TAU = as.vector(mest_df$tau)
    ALPHA = as.vector(mest_df$alpha)
    DELTA = as.vector(mest_df$delta)
  }else{
    LAMBDA = as.matrix(mest_df[-inds_exclude,c("MOT","COG","LANG","SEM")])
    TAU = as.vector(mest_df$tau[-inds_exclude])
    ALPHA = as.vector(mest_df$alpha[-inds_exclude])
    DELTA = as.vector(mest_df$delta[-inds_exclude])
  }



  # Obtain necessary constants
  J = ncol(Y);
  K = 4L
  P = 3L
  N = as.integer(nrow(Y))
  invS = as.matrix(invS)
  SIGMA_SQ= exp(X%*%as.numeric(gamma))


  # initialize the theta values
  THETA0_LF = MU_LF #NxK (matrix)
  THETA0_SF = MU_SF #Nx1 (matrix)

  # Conduct the optimization
  MAP_LF = 0.*THETA0_LF + NA
  MAP_SF = 0.*THETA0_SF + NA
  MAP_OVERALL = 0.*THETA0_SF + NA
  Z_LF = MAP_LF
  SE_LF = MAP_LF
  SE_SF = MAP_SF
  SE_OVERALL = MAP_OVERALL
  NOTES = rep("", nrow(Y))
  writeLines(paste("\nScoring ", N, " observations:"))
  pb<-txtProgressBar(min = 0, max = N, initial = 0, style = 3)

#i = 1
  for (i in 1:N){

    scales_i = which_scores(Y[i,], mest_df)
    if(list_cleaned$is_sf){
      scales_i[-1] <- F
    }

    notes_i = paste0("ID = ", cleaned_df$ID[i],":")
    if(prod(as.numeric(scales_i[1,]))==0){
      if(list_cleaned$is_sf){
        notes_i = paste0(notes_i, " Only responses to short form items detected. Therefore, scoring will produce only a CREDI-SF score.")
        if (scales_i$SF==F){
          notes_i = paste0(notes_i, " Fewer than the number of responses (", min_items,") needed to produce a score.")
        }
      } else {
        notes_i = paste0(notes_i, " The following scales did not have at least ", min_items," responses: ", paste0(names(scales_i)[scales_i[1,]==FALSE], collapse = ", "),"." )
      }

    }


    #Calculate short form scores, as appropriate
    if(scales_i$SF==TRUE){

      js_SF = which(names(Y[i,]) %in% mest_df[mest_df$ShortForm==TRUE,"CREDI_code"])
      out_SF = optim(par = as.vector(THETA0_SF[i,]),
                          fn = sf_posterior_density,
                          Yi = as.vector(Y[i,js_SF]),
                          MUi = as.vector(MU_SF[i,]),
                          SIGMA_SQi = as.numeric(SIGMA_SQ[i]),
                          DELTA = as.vector(DELTA)[js_SF],
                          ALPHA = as.vector(ALPHA)[js_SF],
                          J = length(js_SF), #as.integer(J),
                          method = "BFGS",
                          hessian = TRUE)

      if(out_SF$convergence==0){ #If converged, produce score.
        MAP_SF[i,] = out_SF$par
        SE_SF[i,] = 1/sqrt(out_SF$hessian)
      }  else {
        notes_i = paste0(notes_i, " Scoring procedure for SF scale did not converge; SF score not provided.")
      }
    }

    # Calculate an overall score
    if(scales_i$OVERALL==TRUE){

      js_OVERALL = which(names(Y[i,]) %in% mest_df[mest_df$alpha>0,"CREDI_code"])
      out_OVERALL = optim(par = as.vector(THETA0_SF[i,]),
                          fn = sf_posterior_density,
                          Yi = as.vector(Y[i,js_OVERALL]),
                          MUi = as.vector(MU_SF[i,]),
                          SIGMA_SQi = as.numeric(SIGMA_SQ[i]),
                          DELTA = as.vector(DELTA)[js_OVERALL],
                          ALPHA = as.vector(ALPHA)[js_OVERALL],
                          J = length(js_OVERALL), #as.integer(J),
                          method = "BFGS",
                          hessian = TRUE)

      if(out_OVERALL$convergence==0){ #If converged, produce score.
        MAP_OVERALL[i,] = out_OVERALL$par
        SE_OVERALL[i,] = 1/sqrt(out_OVERALL$hessian)
      } else {
        notes_i = paste0(notes_i, " Scoring procedure for OVERALL scale did not converge; OVERALL score not produced.")
      }


    }

    # Calcuate long form, domain specific subscores
    if(with(scales_i, MOT==T | COG==T | LANG == T | SEM == T)){

      out_LF = optim(par = as.vector(THETA0_LF[i,]),
                     fn = lf_posterior_density,
                     gr = lf_grad_posterior_density,
                     Yi = as.vector(Y[i,]),
                     MUi = as.vector(MU_LF[i,]),
                     invS =invS,
                     TAU = TAU,
                     LAMBDA = LAMBDA,
                     J = J,
                     K = K,
                     method = "BFGS",
                     hessian = TRUE)

      if(out_LF$convergence == 0){
        MAP_LF[i,] = out_LF$par
        SE_LF[i,] = sqrt(diag(solve(out_LF$hessian,diag(K))))

        #Nullify scores for which there are too few items responded in domain
        if(scales_i$MOT==FALSE){MAP_LF[i,"MOT"]<-SE_LF[i,"MOT"]<-NA}
        if(scales_i$COG==FALSE){MAP_LF[i,"COG"]<-SE_LF[i,"COG"]<-NA}
        if(scales_i$LANG==FALSE){MAP_LF[i,"LANG"]<-SE_LF[i,"LANG"]<-NA}
        if(scales_i$SEM==FALSE){MAP_LF[i,"SEM"]<-SE_LF[i,"SEM"]<-NA}

        # Obtain the standardized estimates
        center_i = X_4[i,] %*% as.matrix(normcoef_mean)
        scale_i =  X_4[i,] %*% as.matrix(normcoef_sd)
        Z_LF[i,1:4] = (MAP_LF[i,]+50-center_i[1,1:4])/scale_i[1,1:4]
      } else {
        notes_i = paste0(notes_i, " Multidimensional scoring procedure scale did not converge; subscores not produced.")
      }

    }

    if(!is.null(notes_i)){
      NOTES[i] = notes_i
    }

    setTxtProgressBar(pb, i)
  }

  # Clean up the MAP_LF and SE_LF
  MAP_LF = data.frame(round(MAP_LF,3)+50)
  SE_LF = data.frame(round(SE_LF,3)); names(SE_LF) = paste(names(SE_LF),"_SE", sep = "")
  MAP_LF$OVERALL = round(MAP_OVERALL,3)+50
  MAP_LF$SE_OVERALL = round(SE_OVERALL,3)

  # Clean up the MAP_SF and SE_SF
  MAP_SF = data.frame(SF = round(MAP_SF,3)+50)
  SE_SF = data.frame(SF_SE = round(SE_SF,3))

  #Clean the standardized estimates
  Z_LF = data.frame(round(Z_LF,3))
  names(Z_LF) = paste("z_",names(Z_LF), sep = "")

  # Put in the input
  output_scored = cbind(data.frame(ID = cleaned_df$ID), Z_LF, MAP_LF, MAP_SF,SE_LF, SE_SF, NOTES)
  output_df = merge(x = input_df, y = output_scored, by = "ID") #re-merge with original data.

  # Write out the data
  if(interactive == TRUE){
    out_dlgDir = dlgSave(default = csv_wd, title = "Save scores as", gui = .GUI)
    out_csv = paste(strsplit(out_dlgDir$res,"/")[[1]],collapse = "/")

    if (!endsWith(out_csv,".csv")){out_csv = paste(out_csv, ".csv", sep = "")}
    readr::write_csv(output_df, path = out_csv)

    log[length(log)+1] = paste("\n Scores written to ", out_csv,".", sep = "")

    txt_wd = paste(strsplit(out_csv,"/")[[1]][-length(strsplit(out_csv,"/")[[1]])],collapse = "/")
    out_txt = paste(txt_wd,"/logfile - CREDI scoring.txt", sep = "")
    write_log(log = log, folder = txt_wd, file = out_txt)

    writeLines("\n")
    writeLines( paste("\n Scores written to ", out_csv,".", sep = "") )

  }

  return(list(scores = output_df, log = log))
}
