/*
 *  Power BI Visual CLI
 *
 *  Copyright (c) Microsoft Corporation
 *  All rights reserved.
 *  MIT License
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the ""Software""), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED *AS IS*, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 *  THE SOFTWARE.
 */
module powerbi.extensibility.visual {
    "use strict";
    // below is a snippet of a definition for an object which will contain the property values
    // selected by the users
    /*interface VisualSettings {
        lineColor: string;
    }*/

    // to allow this scenario you should first the following JSON definition to the capabilities.json file
    // under the "objects" property:
    // "settings": {
    //     "displayName": "Visual Settings",
    //     "description": "Visual Settings Tooltip",
    //     "properties": {
    //         "lineColor": {
    //         "displayName": "Line Color",
    //         "type": { "fill": { "solid": { "color": true }}}
    //         }
    //     }
    // }

    // in order to improve the performance, one can update the <head> only in the initial rendering.
    // set to 'true' if you are using different packages to create the widgets
    const updateHTMLHead: boolean = false;
    const renderVisualUpdateType: number[] = [
        VisualUpdateType.Resize,
        VisualUpdateType.ResizeEnd,
        VisualUpdateType.Resize + VisualUpdateType.ResizeEnd
    ];

    export class Visual implements IVisual {
        private rootElement: HTMLElement;
        private headNodes: Node[];
        private bodyNodes: Node[];
        private settings: VisualSettings;

        public constructor(options: VisualConstructorOptions) {
            if (options && options.element) {
                this.rootElement = options.element;
            }
            this.headNodes = [];
            this.bodyNodes = [];
        }

        public update(options: VisualUpdateOptions): void {

            if (!options ||
                !options.type ||
                !options.viewport ||
                !options.dataViews ||
                options.dataViews.length === 0 ||
                !options.dataViews[0]) {
                return;
            }
            const dataView: DataView = options.dataViews[0];
            this.settings = Visual.parseSettings(dataView);

            let payloadBase64: string = null;
            if (dataView.scriptResult && dataView.scriptResult.payloadBase64) {
                payloadBase64 = dataView.scriptResult.payloadBase64;
            }

            if (renderVisualUpdateType.indexOf(options.type) === -1) {
                if (payloadBase64) {
                    this.injectCodeFromPayload(payloadBase64);
                }
            } else {
                this.onResizing(options.viewport);
            }
        }

        public onResizing(finalViewport: IViewport): void {
            /* add code to handle resizing of the view port */
        }

        private injectCodeFromPayload(payloadBase64: string): void {
            // inject HTML from payload, created in R
            // the code is injected to the 'head' and 'body' sections.
            // if the visual was already rendered, the previous DOM elements are cleared

            ResetInjector();

            if (!payloadBase64) {
                return;
            }

            // create 'virtual' HTML, so parsing is easier
            let el: HTMLHtmlElement = document.createElement("html");
            try {
                el.innerHTML = window.atob(payloadBase64);
            } catch (err) {
                return;
            }

            // if 'updateHTMLHead == false', then the code updates the header data only on the 1st rendering
            // this option allows loading and parsing of large and recurring scripts only once.
            if (updateHTMLHead || this.headNodes.length === 0) {
                while (this.headNodes.length > 0) {
                    let tempNode: Node = this.headNodes.pop();
                    document.head.removeChild(tempNode);
                }
                let headList: NodeListOf<HTMLHeadElement> = el.getElementsByTagName("head");
                if (headList && headList.length > 0) {
                    let head: HTMLHeadElement = headList[0];
                    this.headNodes = ParseElement(head, document.head);
                }
            }

            // update 'body' nodes, under the rootElement
            while (this.bodyNodes.length > 0) {
                let tempNode: Node = this.bodyNodes.pop();
                this.rootElement.removeChild(tempNode);
            }
            let bodyList: NodeListOf<HTMLBodyElement> = el.getElementsByTagName("body");
            if (bodyList && bodyList.length > 0) {
                let body: HTMLBodyElement = bodyList[0];
                this.bodyNodes = ParseElement(body, this.rootElement);
            }

            RunHTMLWidgetRenderer();
        }

        private static parseSettings(dataView: DataView): VisualSettings {
            return VisualSettings.parse(dataView) as VisualSettings;
        }

        /** 
         * This function gets called for each of the objects defined in the capabilities files and allows you to select which of the
         * objects and properties you want to expose to the users in the property pane.
         * 
         */
        public enumerateObjectInstances(options: EnumerateVisualObjectInstancesOptions): VisualObjectInstanceEnumeration {
            let objectName = options.objectName;
            let objectEnumeration = [];
            let se = this.settings;

            switch (objectName) {
                case 'mySettingsDet':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            thresholdType: se.mySettingsDet.thresholdType,
                            algName: se.mySettingsDet.algName
                        },
                    });
                    if (se.mySettingsDet.algName == "zscore") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                numSig: se.mySettingsDet.numSig,

                            },
                        });
                    }
                    if (se.mySettingsDet.algName == "manual") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                lThresh: se.mySettingsDet.lThresh,
                                hThresh: se.mySettingsDet.hThresh,

                            },
                        });
                    }
                    if (se.mySettingsDet.algName == "LOF") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                LOFThresh: se.mySettingsDet.LOFThresh,
                                LOFK: se.mySettingsDet.LOFK
                            },
                        });
                    }
                    if (se.mySettingsDet.algName == "Tukey") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                IQR: se.mySettingsDet.IQR
                            },
                        });
                    }
                    if (se.mySettingsDet.algName == "cooks") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                cooksThresh: se.mySettingsDet.cooksThresh
                            },
                        });
                    }
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            toScale: se.mySettingsDet.toScale
                        },
                        selector: null
                    });
                    break;


                case 'mySettingsViz':

                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            plotType: se.mySettingsViz.plotType,
                            visualizeOutlierScore: se.mySettingsViz.visualizeOutlierScore
                        },
                        selector: null
                    });
                    break;
                case 'mySettingsMark':

                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            inlierColor: se.mySettingsMark.inlierColor,
                            outlierColor: se.mySettingsMark.outlierColor,
                            weight: se.mySettingsMark.weight,
                            percentile: se.mySettingsMark.percentile,

                        },
                        selector: null
                    });
                    break;
                case 'mySettingsAxes':

                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            colLabel: se.mySettingsAxes.colLabel,
                            textSize: se.mySettingsAxes.textSize,
                            sizeTicks: se.mySettingsAxes.sizeTicks
                        },
                        selector: null
                    });
                    if (se.mySettingsViz.plotType == "scatter" || se.mySettingsViz.plotType == "density") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                scaleXformat: se.mySettingsAxes.scaleXformat
                            }
                        });
                    }
                    if (se.mySettingsViz.plotType == "scatter" || se.mySettingsViz.plotType == "boxplot") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                scaleYformat: se.mySettingsAxes.scaleYformat
                            }
                        });
                    }
                    break;
                case 'settings_export_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings.settings_export_params.show,
                            limitExportSize: this.settings.settings_export_params.limitExportSize,
                            method: this.settings.settings_export_params.method
                        },
                        selector: null
                    });
                    break;

            };
            //user edit END: populate GUI
            return objectEnumeration;
        }


    }
}