package view

import main.Phonebook
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.control.TextField
import scalafx.scene.layout.VBox
import javafx.event.{ActionEvent, EventHandler}


object App extends JFXApp {
    val phonebook:Phonebook = new Phonebook()

    val search:TextField = new TextField{
        style = "-fx-font: 18 ariel;"
    }

    val outputDisplay:TextField = new TextField{
        style = "-fx-font: 18 ariel;"
    }

    val addButton:Button = new Button{
        minWidth = 100
        minHeight = 100
        style = "-fx-font: 28 ariel;"
        text = "Add"
        onAction = new AddListener(search, outputDisplay, phonebook)
    }
    val deleteButton:Button = new Button{
        minWidth = 100
        minHeight = 100
        style = "-fx-font: 28 ariel;"
        text = "Delete"
        onAction = new DeleteListener(search, phonebook)
    }
    val button:Button = new Button{
        minWidth = 100
        minHeight = 100
        style = "-fx-font: 28 ariel;"
        text = "Lookup"
        onAction = new LookupListener(search, outputDisplay,phonebook)
    }
    val verticalBox = new VBox(){
        children = List(search,outputDisplay,button,addButton,deleteButton)
    }
    this.stage = new PrimaryStage{
        title = "Phonebook"
        scene = new Scene(){
            content = List(verticalBox)
        }
    }
}

class LookupListener(name:TextField, output:TextField, phonebook: Phonebook) extends EventHandler[ActionEvent]{
    override def handle(event: ActionEvent): Unit = {

        val number = phonebook.getNumber(name.text.value)
        output.text.value = number.toString
    }
}
class AddListener(name:TextField, number:TextField, phonebook: Phonebook) extends EventHandler[ActionEvent]{
    override def handle(event: ActionEvent): Unit = {
        phonebook.addContact(name.text.value, number.text.value.toLong)
        name.text.value = ""
        number.text.value = ""
    }
}
class DeleteListener(name:TextField, phonebook: Phonebook) extends EventHandler[ActionEvent]{
    override def handle(event: ActionEvent): Unit = {
        phonebook.deleteContact(name.text.value)
        name.text.value = ""
    }
}