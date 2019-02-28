## Import packages
from flask import Flask, request, jsonify
import pandas

## Initialize app
app = Flask(__name__)


predictions = pandas.read_csv(r"C:\Users\aparihar\Documents\GitHub\recommendation_systems\Book1.csv")

## How this is done in Spark (information)
#movies = spark.read.option("header", "true").csv("movies.csv")\
#                    .select("movieId", "title")\
#                    .repartition("movieId")
            
#predictions = predictions.join(movies, "movieId", "left")\
#                    .orderBy(col("userId"), col("prediction").desc())\
#                    .cache()

## The application definiton
### Endpoint - one route /ratings/top - one HTTP verb = POST
@app.route("/ratings/top", methods=["POST"])
def top_ratings():
    ## read the body of the API call
    content = request.get_json()
    
    ## Interpretation of body
    if "userID" in content and type(content["userID"]) == int:
        userID = content["userID"]
    else:
        return "'userID' is required and should be an Integer."
        sys.exit("'userID' is required and should be an Integer.")
        
    if "count" in content and type(content["count"]) == int:
        count = content["count"]
    else:
        count = 5
    
    # filter predictions for the given userId
    predict = predictions[predictions.userID == userID].head(count)
    
    # select movieId, title and prediction and transform to list
    top_ratings = list(predict[["userID", "artistID","name"]].T.to_dict().values())
    
    # Return the result to the API
    return jsonify(top_ratings)

### Put endpoint online
if __name__ == '__main__':
    app.run(host='localhost', port=6000)