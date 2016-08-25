# Emma Jupyter Notebooks

To run the notebooks, you need the following software installed on your machine:

 - The [Jupyter Project](http://jupyter.org/) itself, and
 - [Jupyter Scala](https://github.com/alexarchambault/jupyter-scala/) for the Scala kernel.

To see the notebooks, clone or download this project and run the following commands:

 ```bash
 cd emma # Replace with custom directory if necessary.
 mvn clean install -DskipTests # NOTE: Might take up to 15 min.
 cd emma-jupyter
 jupyter notebook # Run the Jupyter notebook.
 ```
