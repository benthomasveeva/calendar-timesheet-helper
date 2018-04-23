import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const CLIENT_ID = "159193416244-l4tsfgdhbn402qq57ajahsf3cu41vno0.apps.googleusercontent.com";
const SCOPE = "email profile https://www.googleapis.com/auth/calendar";
const DISCOVERY_DOCS = ["https://www.googleapis.com/discovery/v1/apis/calendar/v3/rest"];

const app = Main.embed(document.getElementById('root'));

registerServiceWorker();

window.onGoogleYoloLoad = (googleyolo) => {
    app.ports.jsToElm.send({ "msg": "googleyoloready" });
    console.log(googleyolo);
}
app.ports.elmToJs.subscribe((elmMsg) => {
    if (elmMsg.msg === "login") {
        googleyolo.retrieve({
            supportedAuthMethods: ["https://accounts.google.com"],
            supportedIdTokenProviders: [{ uri: "https://accounts.google.com", clientId: "159193416244-l4tsfgdhbn402qq57ajahsf3cu41vno0.apps.googleusercontent.com" }]
        }).then((credential) => {
            app.ports.jsToElm.send({ "msg": "credentialsuccess", "idToken": credential.idToken, "email": credential.id });
        }, (error) => {
            console.log("error", error);
            app.ports.jsToElm.send({ "msg": "credentialfail", "error": error });
        });
    } else if (elmMsg.msg === "loginHint") {
        googleyolo.hint({
            supportedAuthMethods: ["https://accounts.google.com"],
            supportedIdTokenProviders: [{ uri: "https://accounts.google.com", clientId: "159193416244-l4tsfgdhbn402qq57ajahsf3cu41vno0.apps.googleusercontent.com" }]
        }).then((credential) => {
            app.ports.jsToElm.send({ "msg": "credentialsuccess", "idToken": credential.idToken, "email": credential.id });
        }, (error) => {
            console.log("error", error);
            app.ports.jsToElm.send({ "msg": "credentialfail", "error": error });
        });
    } else if (elmMsg.msg === "loadapi") {
        gapi.load('client', () => {
            return gapi.auth2.authorize({
                clientId: CLIENT_ID,
                prompt: 'none',
                response_type: 'permission',
                scope: SCOPE,
                login_hint: elmMsg.email
            }, (gapiauthorize) => {
                gapi.client.init({ discoveryDocs: DISCOVERY_DOCS }).then(() => {
                    app.ports.jsToElm.send({ "msg": "gapiready" });
                })
            });
        });
    } else if (elmMsg.msg === "loadCalendars") {
        gapi.client.calendar.calendarList.list().then((resp) => {
            app.ports.jsToElm.send({ "msg": "calendarlist", "success": true, "items": resp.result.items });
        }, (err) => {
            console.log(err);
            app.ports.jsToElm.send({ "msg": "calendarlist", "success": false });
        });
    } else if (elmMsg.msg === "loadColors") {
        gapi.client.calendar.colors.get().then((resp) => {
            app.ports.jsToElm.send({ "msg": "colors", "eventColors": resp.result.event })
        }, (err) => {
            console.log(err);
            app.ports.jsToElm.send({ "msg": "colors", "error": err });
        });
    } else if (elmMsg.msg === "loadEvents") {
        gapi.client.calendar.events.list({ calendarId: elmMsg.calendar, singleEvents: true, timeMin: elmMsg.timeMin, timeMax: elmMsg.timeMax }).then((resp) => {
            app.ports.jsToElm.send({ "msg": "events", "events": resp.result.items });
        }, (err) => {
            console.log(err);
            app.ports.jsToElm.send({ "msg": "events", "error": err });
        });
    } else if (elmMsg.msg === "createEvent") {
        gapi.client.calendar.events.insert({
            calendarId: elmMsg.calendar,
            colorId: "11",
            summary: elmMsg.eventName,
            visibility: "private",
            transparency: "transparent",
            start: { dateTime: elmMsg.startTime },
            end: { dateTime: elmMsg.endTime }
        }).then((resp) => {
            app.ports.jsToElm.send({ "msg": "eventcreated" });
        }, (err) => {
            console.log(err);
            app.ports.jsToElm.send({ "msg": "eventcreated", "error": err });
        });
    } else if (elmMsg.msg === "changeEventColor") {
        gapi.client.calendar.events.patch({
            calendarId: elmMsg.calendar,
            eventId: elmMsg.event,
            colorId: elmMsg.color
        }).then((resp) => {
            app.ports.jsToElm.send({ "msg": "eventupdated", "eventId": elmMsg.event, "success": true });
        }, (error) => {
            console.log(error);
            app.ports.jsToElm.send({ "msg": "eventupdated", "eventId": elmMsg.event, "error": error })
        });
    }
});
