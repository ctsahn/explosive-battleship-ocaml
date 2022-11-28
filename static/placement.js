// do this in different file TODO
async function handleShipPlacement(cellid, formid){

    document.getElementById(cellid).style.backgroundColor = "purple";
        
    document.getElementById(formid).submit();
  
  
}

function loadColors(){
    let userBoardStatus = document.getElementById("user-board-status").innerHTML;
    
    for(let i=0;i<100;i++){
        
        if(userBoardStatus[i] == 2){
          document.getElementById("usercell" + i).style.backgroundColor = "purple";
        }
      
    }

    let shipStatus = document.getElementById("ship-status").innerHTML;

    for(let i=0;i<shipStatus.length;i++){
        console.log(shipStatus.charAt(i));
            
        document.getElementById("length" + shipStatus.charAt(i)).style.color = "red";

    }

    let shipSize = document.getElementById("placed-ship-size").innerHTML;

    // not 0
    if(shipSize>0){
        console.log(shipSize);
        document.getElementById("length" + shipSize).style.color = "red";
    }

    

}
loadColors(); // we want to call this every time we load a page