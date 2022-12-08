let disable = false; //disable multiple clicks
async function handleClick(cellid,formid){
    // only allow clicks when it is your turn, and make sure a form exists in order for click to occur
    if(!disable && document.getElementById(formid) && !document.getElementById("cpu-turn")){

    

      document.getElementById(cellid).style.backgroundColor = "red";
      
      document.getElementById(formid).submit();
      disable=true;
    }


}

// when CPU turn, automatically send a request to the /cpu_turn endpoint
async function handleCPUTurn(){
  if(document.getElementById("cpu-turn")){
    document.getElementById("cpu-turn").submit();

  }

}
function loadColors(){
    
    let userBoardStatus = document.getElementById("user-board-status").innerHTML;
    let cpuBoardStatus = document.getElementById("cpu-board-status").innerHTML;
    for(let i=0;i<100;i++){
        
        // miss
        if(userBoardStatus[i] == 1){
          document.getElementById("usercell" + i).innerHTML = "•";
        }
        // ship
        else if(userBoardStatus[i] == 2){
          document.getElementById("usercell" + i).style.backgroundColor = "blue";
        }
        // hit
        else if(userBoardStatus[i] == 3){
          document.getElementById("usercell" + i).innerHTML = "X";
          document.getElementById("usercell" + i).style.color = "red";
          document.getElementById("usercell" + i).style.borderColor = "red";
          
        }
        else if(userBoardStatus[i]==4){
          document.getElementById("usercell" + i).style.backgroundColor = "red";
          document.getElementById("usercell" + i).style.color = "red";
          document.getElementById("usercell" + i).style.borderColor = "red";
        } 

        // sunk


        // miss
        if(cpuBoardStatus[i] == 1){
          document.getElementById("cpucell" + i).innerHTML= "•";
        }
        // ship
        else if(cpuBoardStatus[i] == 2){
          document.getElementById("cpucell" + i).style.backgroundColor = "blue";
        }
        // hit
        else if(cpuBoardStatus[i] == 3){
          
          document.getElementById("cpucell" + i).innerHTML = "X";
          document.getElementById("cpucell" + i).style.color = "red";
          document.getElementById("cpucell" + i).style.borderColor = "red";
        }
        else if(cpuBoardStatus[i]==4){
          document.getElementById("cpucell" + i).style.backgroundColor = "red";
          document.getElementById("cpucell" + i).style.color = "red";
          document.getElementById("cpucell" + i).style.borderColor = "red";
        } 
      
    }

}
loadColors(); // we want to call this every time we load a page