#ifndef MAINWIDGET_H
#define MAINWIDGET_H

#include "ui_MainWidget.h"

class MainWidget : public QWidget
{
    Q_OBJECT

public:
    explicit MainWidget(QWidget *parent = 0);

private:
    Ui::MainWidget ui;
};

#endif // MAINWIDGET_H
