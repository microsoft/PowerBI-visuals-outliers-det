/*
 *  Power BI Visualizations
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

    import DataViewObjectsParser = powerbi.extensibility.utils.dataview.DataViewObjectsParser;

   export class VisualSettings extends DataViewObjectsParser {
     // public rcv_script: rcv_scriptSettings = new rcv_scriptSettings();
      
      public mySettingsDet: mySettingsDet = new mySettingsDet();
      public mySettingsViz: mySettingsViz = new mySettingsViz();
      public mySettingsMark: mySettingsMark = new mySettingsMark();
      public mySettingsAxes: mySettingsAxes = new mySettingsAxes();
    //  public mySettingsTransf: mySettingsTransf = new mySettingsTransf();
      }
 
 
      export class mySettingsDet {
        public thresholdType: string = "PeaksAndSubpeaks";
        public algName: string = "zscore";
        public numSig: string = "4";
        public IQR: string =  "3";
        public LOFThresh: string = "3";
        public LOFK: string = "3";
        public lThresh: number = -100;
        public hThresh: number = 100;
        public cooksThresh: string = "5";
        public toScale: boolean = true;
    }
    
      export class mySettingsViz {
        public plotType: string = "scatter";
       public googleVizScatter: boolean = false;
       public visualizeOutlierScore: boolean = false;	
      	    
    }
    export class mySettingsMark {
      public inlierColor: string = "blue";
      public outlierColor: string = "red";
      public weight: number = 10;
      public sparsify: boolean = true;
      public percentile: number = 40;

  }
  export class mySettingsAxes {
    public colLabel: string = "gray";
    public textSize: number = 10;
    public scaleXformat: string = "none";
    public scaleYformat: string = "none";
    public sizeTicks: string = "8";

}

    export class mySettingsTransf {
      public algTransf: string = "mean";
      public manualVal: number = 0;
  }

    export class rcv_scriptSettings {
     // undefined
      public provider     // undefined
      public source     }

}
