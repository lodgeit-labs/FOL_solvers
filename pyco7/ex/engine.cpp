#include <QTextStream>
#include <QFile>
#include <QJsonArray>
#include <QJsonDocument>
#include "engine.h"

Engine::Engine()
{


}

void Engine::load()
{
    QFile input1;
    input1.open(stdin, QIODevice::ReadOnly);
    auto input2 = input1.readAll();
    QJsonParseError error;
    auto doc = QJsonDocument::fromJson(input2, &error);
    QTextStream errorStream(stderr);
    if (doc.isNull())
    {

        errorStream << "error reading JSON from stdin: ";
        errorStream << error.errorString();
        errorStream << "\n";
    }
    else
    {
        errorStream << doc.toJson();
        errorStream << "\n";


    }

}
