package controllers

import akka.actor.ActorSystem
import javax.inject.Inject
import models.Widget
import play.api.Configuration
import play.api.data._
import play.api.libs.ws.{WSClient, WSRequest, WSResponse}
import play.api.mvc._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import play.api.libs.json.JsString


/**
 * The classic WidgetController using MessagesAbstractController.
 *
 * Instead of MessagesAbstractController, you can use the I18nSupport trait,
 * which provides implicits that create a Messages instance from a request
 * using implicit conversion.
 *
 * See https://www.playframework.com/documentation/2.6.x/ScalaForms#passing-messagesprovider-to-form-helpers
 * for details.
 */
class WidgetController @Inject()(cc: MessagesControllerComponents, actorSystem: ActorSystem, ws: WSClient, config: Configuration)(implicit exec: ExecutionContext) extends MessagesAbstractController(cc) {
  import WidgetForm._

  val obpApiHostname = config.get[String]("obp.api.hostname").replaceFirst("/$", "")
  val requestTokenUrl = s"${obpApiHostname}/my/logins/direct"

  // The URL to the widget.  You can call this directly from the template, but it
  // can be more convenient to leave the template completely stateless i.e. all
  // of the "WidgetController" references are inside the .scala file.
  private val postUrl = routes.WidgetController.createWidget()

  def index = Action {
    Ok(views.html.index())
  }

  def listWidgets = Action { implicit request: MessagesRequest[AnyContent] =>
    // Pass an unpopulated form to the template
    Ok(views.html.listWidgets(form, postUrl))
  }

  // This will be the action that handles our form post
  def createWidget = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[Data] =>
      // This is the bad case, where the form had validation errors.
      // Let's show the user the form again, with the errors highlighted.
      // Note how we pass the form with errors to the template.
      BadRequest(views.html.listWidgets(formWithErrors, postUrl))
    }

    val successFunction = { data: Data =>
      // This is the good case, where the form was successfully parsed as a Data object.
      val widget = Widget(username = data.username, password = data.password, consumer_key = data.consumer_key)
//      val tokenFuture = requestToken("admin", "Baishuang`12", "mtta3aa3hlt1mxnx2d400eacst43smag2retnpvo")
      val tokenFuture = requestToken(data.username, data.password, data.consumer_key)
      val result = Await.result(tokenFuture, 3.seconds)
      val name = if(result.startsWith("OBP-")) "errorMsg" else "token"
      Redirect(routes.WidgetController.listWidgets()).flashing(name -> result)
    }

    val formValidationResult = form.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }

  private def requestToken(username: String, password: String, consumerKey: String): Future[String] = {
    val request: WSRequest = ws.url(requestTokenUrl)
    val complexRequest: WSRequest =
      request.addHttpHeaders(
        "Accept" -> "application/json",
        "Content-Type" -> "application/json",
        "Authorization" -> s"""DirectLogin username="${username}",password="${password}",consumer_key="${consumerKey}""""
      ).withRequestTimeout(10000.millis)
    val responseFuture: Future[WSResponse] = complexRequest.post("")
    responseFuture.map { it =>
      it.status match {
        case CREATED => it.json.apply("token")
        case _ => it.json.apply("message")
      }
    }
    .map(it => it.asInstanceOf[JsString].value)
  }
}
