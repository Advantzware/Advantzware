using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

public partial class corr_layout : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
               
        LayoutDataSource.SelectParameters["prmAction"].DefaultValue = "Select";
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "corr_layout.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;


            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Label compname = (Label)Master.FindControl("lblComp");
            Label username = (Label)Master.FindControl("lblUser");
            Label labelname = (Label)Master.FindControl("lbl_page");
            compname.Text = PrmComp;
            username.Text = UserLogin.UserName;
            labelname.Text = "Corrugated";

            if (aUsers == "external")
            {

            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        try
        {
            Label type = (Label)FormView_Layout.FindControl("vTypeLabel");
            Session["corr_type_val"] = type.Text;
            
            /*ImageButton layout = (ImageButton)Master.FindControl("img_layout");
            layout.ImageUrl = "~/Images/layout_1.jpg";*/
            Image img_mov_col = (Image)Master.FindControl("Image5");
            img_mov_col.Visible = false;
        }
        catch { }
        
    }

    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Label compname = (Label)Master.FindControl("lblComp");

        TextBox machine = (TextBox)FormView_Layout.FindControl("vMachineTextBox");
        Label machineDesc = (Label)FormView_Layout.FindControl("vMachDscrTextBox");
        TextBox frontback = (TextBox)FormView_Layout.FindControl("vFrontBackTextBox");
        TextBox sideside = (TextBox)FormView_Layout.FindControl("vSideSideTextBox");
        DropDownList revcorr = (DropDownList)FormView_Layout.FindControl("vRevCorrDropDown");
        TextBox board = (TextBox)FormView_Layout.FindControl("vBoardTextBox");
        TextBox boardname = (TextBox)FormView_Layout.FindControl("vBoardNameTextBox");
        TextBox costuom = (TextBox)FormView_Layout.FindControl("vCostUomTextBox");
        TextBox costmsf = (TextBox)FormView_Layout.FindControl("vCostMsfTextBox");
        TextBox freightuom = (TextBox)FormView_Layout.FindControl("vFreightUomTextBox");
        TextBox freightmsf = (TextBox)FormView_Layout.FindControl("vFreightCwtTextBox");
        //TextBox nc = (TextBox)FormView_Layout.FindControl("vNcTextBox");
        DropDownList dnc = (DropDownList)FormView_Layout.FindControl("ddl_nc");
        TextBox grosshetwid = (TextBox)FormView_Layout.FindControl("vGrosShetWidTextBox");
        TextBox grosshetlen = (TextBox)FormView_Layout.FindControl("vGrosShetLenTextBox");
        TextBox outwid = (TextBox)FormView_Layout.FindControl("vOutWidTextBox");
        TextBox outlen = (TextBox)FormView_Layout.FindControl("vOutLenTextBox");
        TextBox outcut = (TextBox)FormView_Layout.FindControl("vOutCutTextBox");
        TextBox dieinches = (TextBox)FormView_Layout.FindControl("vDieInchesTextBox");
        TextBox netshetwid = (TextBox)FormView_Layout.FindControl("vNetShetWidTextBox");
        TextBox netshetlen = (TextBox)FormView_Layout.FindControl("vNetShetLenTextBox");
        TextBox diesizewid = (TextBox)FormView_Layout.FindControl("vDieSizeWidTextBox");
        TextBox diesizelen = (TextBox)FormView_Layout.FindControl("vDieSizeLenTextBox");
        TextBox onwid = (TextBox)FormView_Layout.FindControl("vOnWidTextBox");
        TextBox onlen = (TextBox)FormView_Layout.FindControl("vOnLenTextBox");
        TextBox adder1 = (TextBox)FormView_Layout.FindControl("vAdders1TextBox");
        TextBox adder2 = (TextBox)FormView_Layout.FindControl("vAdders2TextBox");
        TextBox adder3 = (TextBox)FormView_Layout.FindControl("vAdders3TextBox");
        TextBox adder4 = (TextBox)FormView_Layout.FindControl("vAdders4TextBox");
        TextBox adder5 = (TextBox)FormView_Layout.FindControl("vAdders5TextBox");
        TextBox adder6 = (TextBox)FormView_Layout.FindControl("vAdders6TextBox");
        TextBox adder7 = (TextBox)FormView_Layout.FindControl("vAdders7TextBox");
        TextBox adder8 = (TextBox)FormView_Layout.FindControl("vAdders8TextBox");
        TextBox adder9 = (TextBox)FormView_Layout.FindControl("vAdders9TextBox");
        TextBox adder10 = (TextBox)FormView_Layout.FindControl("vAdders10TextBox");
        TextBox adder11 = (TextBox)FormView_Layout.FindControl("vAdders11TextBox");
        TextBox adder12 = (TextBox)FormView_Layout.FindControl("vAdders12TextBox");
        TextBox waxlabel1 = (TextBox)FormView_Layout.FindControl("vWaxLabel1TextBox");
        TextBox waxdesc1 = (TextBox)FormView_Layout.FindControl("vWaxDesc1TextBox");
       // TextBox s1 = (TextBox)FormView_Layout.FindControl("vLeafS1TextBox");
        TextBox b1 = (TextBox)FormView_Layout.FindControl("vLeafB1TextBox");
        TextBox leafwid1 = (TextBox)FormView_Layout.FindControl("vLeafWid1TextBox");
        TextBox leaflen1 = (TextBox)FormView_Layout.FindControl("vLeafLen1TextBox");
        TextBox waxlabel2 = (TextBox)FormView_Layout.FindControl("vWaxLabel2TextBox");
        TextBox waxdesc2 = (TextBox)FormView_Layout.FindControl("vWaxDesc2TextBox");
       // TextBox s2 = (TextBox)FormView_Layout.FindControl("vLeafS2TextBox");
        TextBox b2 = (TextBox)FormView_Layout.FindControl("vLeafB2TextBox");
        TextBox leafwid2 = (TextBox)FormView_Layout.FindControl("vLeafWid2TextBox");
        TextBox leaflen2 = (TextBox)FormView_Layout.FindControl("vLeafLen2TextBox");
        TextBox waxlabel3 = (TextBox)FormView_Layout.FindControl("vWaxLabel3TextBox");
        TextBox waxdesc3 = (TextBox)FormView_Layout.FindControl("vWaxDesc3TextBox");
       // TextBox s3 = (TextBox)FormView_Layout.FindControl("vLeafS3TextBox");
        TextBox b3 = (TextBox)FormView_Layout.FindControl("vLeafB3TextBox");
        TextBox leafwid3 = (TextBox)FormView_Layout.FindControl("vLeafWid3TextBox");
        TextBox leaflen3 = (TextBox)FormView_Layout.FindControl("vLeafLen3TextBox");
        TextBox waxlabel4 = (TextBox)FormView_Layout.FindControl("vWaxLabel4TextBox");
        TextBox waxdesc4 = (TextBox)FormView_Layout.FindControl("vWaxDesc4TextBox");
       // TextBox s4 = (TextBox)FormView_Layout.FindControl("vLeafS4TextBox");
        TextBox b4 = (TextBox)FormView_Layout.FindControl("vLeafB4TextBox");
        TextBox leafwid4 = (TextBox)FormView_Layout.FindControl("vLeafWid4TextBox");
        TextBox leaflen4 = (TextBox)FormView_Layout.FindControl("vLeafLen4TextBox");
        Label blankwid=(Label)FormView_Layout.FindControl("vBlankWidLabel");
        Label blanklen = (Label)FormView_Layout.FindControl("vBlankLenLabel");
        Label real = (Label)FormView_Layout.FindControl("vRealLabel");
        Label flute = (Label)FormView_Layout.FindControl("vFluteLabel");
        Label test = (Label)FormView_Layout.FindControl("vTestLabel");
        Label ontotalup = (Label)FormView_Layout.FindControl("vOnTotalUpLabel");

        if (frontback.Text == "")
            frontback.Text = "0";
        if (sideside.Text == "")
            sideside.Text = "0";
        if (costmsf.Text == "")
            costmsf.Text = "0";
        if (freightmsf.Text == "")
            freightmsf.Text = "0";
        if (grosshetwid.Text == "")
            grosshetwid.Text = "0";
        if (grosshetlen.Text == "")
            grosshetlen.Text = "0";
        if (outwid.Text == "")
            outwid.Text = "0";
        if (outlen.Text == "")
            outlen.Text = "0";
        if (outcut.Text == "")
            outcut.Text = "0";
        if (dieinches.Text == "")
            dieinches.Text = "0";
        if (netshetwid.Text == "")
            netshetwid.Text = "0";
        if (netshetlen.Text == "")
            netshetlen.Text = "0";
        if (diesizewid.Text == "")
            diesizewid.Text = "0";
        if (diesizelen.Text == "")
            diesizelen.Text = "0";
        if (onwid.Text == "")
            onwid.Text = "0";
        if (onlen.Text == "")
            onlen.Text = "0";
        if (blankwid.Text == "")
            blankwid.Text = "0";
        if(blanklen.Text=="")
            blanklen.Text="0";
        if(b1.Text=="")
            b1.Text="0";
        if(leafwid1.Text=="")
            leafwid1.Text="0";                                                                                                                                                                                               
        if(leaflen1.Text=="")
           leaflen1.Text="0";                                                                               
        if( b2.Text=="")
            b2.Text="0";                                                                               
        if(leafwid2.Text=="")
             leafwid2.Text="0";                                                                       
        if(   leaflen2.Text=="")
               leaflen2.Text="0";                                                                       
        if(    b3.Text=="")
            b3.Text="0";                                                                       
        if(leafwid3.Text=="")
             leafwid3.Text="0";                                                                           
        if(leaflen3.Text=="")
             leaflen3.Text="0";                                                                           
        if(b4.Text=="")
             b4.Text="0";                                                                                                          
        if(leafwid4.Text=="")
            leafwid4.Text="0";
        if (leaflen4.Text == "")
            leaflen4.Text = "0";                                                                                    
                                                                                             
                                                                                 

                                                                                       
                                                                               
                                                                          

                                                                                 
                                                                            
                                                                                  

        
        Corrugated corr = new Corrugated();

        bool check = corr.ValidateCorrLayout(Convert.ToString(Session["User"]), "ValidateOverRide", "", "", Convert.ToString(Session["order_corrugated_est"]), Convert.ToDateTime("12/11/2009"), Convert.ToInt32(Session["order_corrugated_formno"]), 0, "", machine.Text.Trim(), "", Convert.ToDecimal(frontback.Text.Trim()), Convert.ToDecimal(sideside.Text.Trim()), revcorr.SelectedValue, board.Text, "", "", flute.Text, test.Text, Convert.ToDecimal(costmsf.Text.Trim()), costuom.Text, 0, Convert.ToDecimal(freightmsf.Text), freightuom.Text, dnc.SelectedValue, Convert.ToDecimal(grosshetwid.Text), Convert.ToDecimal(grosshetlen.Text), 0, Convert.ToDecimal(outwid.Text), Convert.ToDecimal(outlen.Text), 0, Convert.ToInt32(outcut.Text), "", "", Convert.ToDecimal(dieinches.Text), Convert.ToDecimal(netshetwid.Text), Convert.ToDecimal(netshetlen.Text), 0, Convert.ToDecimal(diesizewid.Text), Convert.ToDecimal(diesizelen.Text), 0, Convert.ToDecimal(onwid.Text), Convert.ToDecimal(onlen.Text), 0, 0, 0, Convert.ToDecimal(blankwid.Text), Convert.ToDecimal(blanklen.Text), 0, adder1.Text, adder2.Text, adder3.Text, adder4.Text, adder5.Text, adder6.Text, adder7.Text, adder8.Text, adder9.Text, adder10.Text, adder11.Text, adder12.Text, waxlabel1.Text, waxdesc1.Text, 0, Convert.ToInt32(b1.Text), Convert.ToDecimal(leafwid1.Text), Convert.ToDecimal(leaflen1.Text), waxlabel2.Text, waxdesc2.Text, 0, Convert.ToInt32(b2.Text), Convert.ToDecimal(leafwid2.Text), Convert.ToDecimal(leaflen2.Text), waxlabel3.Text, waxdesc3.Text, 0, Convert.ToInt32(b3.Text), Convert.ToDecimal(leafwid3.Text), Convert.ToDecimal(leaflen3.Text), waxlabel4.Text, waxdesc4.Text, 0, Convert.ToInt32(b4.Text), Convert.ToDecimal(leafwid4.Text), Convert.ToDecimal(leaflen4.Text), Convert.ToInt32(Session["order_corrugated_blankno"]));          

        string value = Convert.ToString(check);

        if (value == "True")
        {


            LayoutDataSource.SelectParameters["prmAction"].DefaultValue = "OverRide";
            LayoutDataSource.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            LayoutDataSource.SelectParameters["prmComp"].DefaultValue = compname.Text;
            LayoutDataSource.SelectParameters["prmEstimate"].DefaultValue = Convert.ToString(Session["order_corrugated_est"]);
            LayoutDataSource.SelectParameters["prmForm"].DefaultValue = Convert.ToString(Session["order_corrugated_formno"]);
            LayoutDataSource.SelectParameters["prmMachine"].DefaultValue = machine.Text.Trim();
            LayoutDataSource.SelectParameters["prmMachDscr"].DefaultValue = machineDesc.Text.Trim();
            LayoutDataSource.SelectParameters["prmFrontBack"].DefaultValue = frontback.Text.Trim();
            LayoutDataSource.SelectParameters["prmSideSide"].DefaultValue = sideside.Text.Trim();
            LayoutDataSource.SelectParameters["prmRevCorr"].DefaultValue = revcorr.SelectedValue.ToString();
            LayoutDataSource.SelectParameters["prmBoard"].DefaultValue = board.Text.Trim();
            LayoutDataSource.SelectParameters["prmBoardName"].DefaultValue = boardname.Text.Trim();
            LayoutDataSource.SelectParameters["prmCostMsf"].DefaultValue = costmsf.Text.Trim();
            LayoutDataSource.SelectParameters["prmCostUom"].DefaultValue = costuom.Text.Trim();
            LayoutDataSource.SelectParameters["prmFreightMsf"].DefaultValue = freightmsf.Text.Trim();
            LayoutDataSource.SelectParameters["prmFreightUom"].DefaultValue = freightuom.Text.Trim();
            LayoutDataSource.SelectParameters["prmNc"].DefaultValue = dnc.SelectedValue.ToString();
            LayoutDataSource.SelectParameters["prmReal"].DefaultValue = real.Text.Trim();
            LayoutDataSource.SelectParameters["prmFlute"].DefaultValue = flute.Text.Trim();
            LayoutDataSource.SelectParameters["prmTest"].DefaultValue = test.Text.Trim();
            LayoutDataSource.SelectParameters["prmGrosShetWid"].DefaultValue = grosshetwid.Text.Trim();
            LayoutDataSource.SelectParameters["prmGrosShetLen"].DefaultValue = grosshetlen.Text.Trim();
            LayoutDataSource.SelectParameters["prmOutWid"].DefaultValue = outwid.Text.Trim();
            LayoutDataSource.SelectParameters["prmOutLen"].DefaultValue = outlen.Text.Trim();
            LayoutDataSource.SelectParameters["prmOutCut"].DefaultValue = outcut.Text.Trim();
            LayoutDataSource.SelectParameters["prmDieInches"].DefaultValue = dieinches.Text.Trim();
            LayoutDataSource.SelectParameters["prmNetShetWid"].DefaultValue = netshetwid.Text.Trim();
            LayoutDataSource.SelectParameters["prmNetShetLen"].DefaultValue = netshetlen.Text.Trim();
            LayoutDataSource.SelectParameters["prmDieSizeWid"].DefaultValue = diesizewid.Text.Trim();
            LayoutDataSource.SelectParameters["prmDieSizeLen"].DefaultValue = diesizelen.Text.Trim();
            LayoutDataSource.SelectParameters["prmBlankWid"].DefaultValue = blankwid.Text.Trim();
            LayoutDataSource.SelectParameters["prmBlankLen"].DefaultValue = blanklen.Text.Trim();
            LayoutDataSource.SelectParameters["prmOnWid"].DefaultValue = onwid.Text.Trim();
            LayoutDataSource.SelectParameters["prmOnLen"].DefaultValue = onlen.Text.Trim();
            LayoutDataSource.SelectParameters["prmAdder1"].DefaultValue = adder1.Text.Trim();
            LayoutDataSource.SelectParameters["prmAdder2"].DefaultValue = adder2.Text.Trim();
            LayoutDataSource.SelectParameters["prmAdder3"].DefaultValue = adder3.Text.Trim();
            LayoutDataSource.SelectParameters["prmAdder4"].DefaultValue = adder4.Text.Trim();
            LayoutDataSource.SelectParameters["prmAdder5"].DefaultValue = adder5.Text.Trim();
            LayoutDataSource.SelectParameters["prmAdder6"].DefaultValue = adder6.Text.Trim();
            LayoutDataSource.SelectParameters["prmAdder7"].DefaultValue = adder7.Text.Trim();
            LayoutDataSource.SelectParameters["prmAdder8"].DefaultValue = adder8.Text.Trim();
            LayoutDataSource.SelectParameters["prmAdder9"].DefaultValue = adder9.Text.Trim();
            LayoutDataSource.SelectParameters["prmAdder10"].DefaultValue = adder10.Text.Trim();
            LayoutDataSource.SelectParameters["prmAdder11"].DefaultValue = adder11.Text.Trim();
            LayoutDataSource.SelectParameters["prmAdder12"].DefaultValue = adder12.Text.Trim();
            LayoutDataSource.SelectParameters["prmWaxLabel1"].DefaultValue = waxlabel1.Text.Trim();
            LayoutDataSource.SelectParameters["prmWaxDesc1"].DefaultValue = waxdesc1.Text.Trim();
            // LayoutDataSource.SelectParameters["prmS1"].DefaultValue = s1.Text.Trim();
            LayoutDataSource.SelectParameters["prmB1"].DefaultValue = b1.Text.Trim();
            LayoutDataSource.SelectParameters["prmLeafWid1"].DefaultValue = leafwid1.Text.Trim();
            LayoutDataSource.SelectParameters["prmLeafLen1"].DefaultValue = leaflen1.Text.Trim();
            LayoutDataSource.SelectParameters["prmWaxLabel2"].DefaultValue = waxlabel2.Text.Trim();
            LayoutDataSource.SelectParameters["prmWaxDesc2"].DefaultValue = waxdesc2.Text.Trim();
            //  LayoutDataSource.SelectParameters["prmS2"].DefaultValue = s2.Text.Trim();
            LayoutDataSource.SelectParameters["prmB2"].DefaultValue = b2.Text.Trim();
            LayoutDataSource.SelectParameters["prmLeafWid2"].DefaultValue = leafwid2.Text.Trim();
            LayoutDataSource.SelectParameters["prmLeafLen2"].DefaultValue = leaflen2.Text.Trim();
            LayoutDataSource.SelectParameters["prmWaxLabel3"].DefaultValue = waxlabel3.Text.Trim();
            LayoutDataSource.SelectParameters["prmWaxDesc3"].DefaultValue = waxdesc3.Text.Trim();
            //    LayoutDataSource.SelectParameters["prmS3"].DefaultValue = s3.Text.Trim();
            LayoutDataSource.SelectParameters["prmB3"].DefaultValue = b3.Text.Trim();
            LayoutDataSource.SelectParameters["prmLeafWid3"].DefaultValue = leafwid3.Text.Trim();
            LayoutDataSource.SelectParameters["prmLeafLen3"].DefaultValue = leaflen3.Text.Trim();
            LayoutDataSource.SelectParameters["prmWaxLabel4"].DefaultValue = waxlabel4.Text.Trim();
            LayoutDataSource.SelectParameters["prmWaxDesc4"].DefaultValue = waxdesc4.Text.Trim();
            //   LayoutDataSource.SelectParameters["prmS4"].DefaultValue = s4.Text.Trim();
            LayoutDataSource.SelectParameters["prmB4"].DefaultValue = b4.Text.Trim();
            LayoutDataSource.SelectParameters["prmLeafWid4"].DefaultValue = leafwid4.Text.Trim();
            LayoutDataSource.SelectParameters["prmLeafLen4"].DefaultValue = leaflen4.Text.Trim();
            LayoutDataSource.SelectParameters["prmOnTotalUp"].DefaultValue = ontotalup.Text.Trim();
            
            FormView_Layout.ChangeMode(FormViewMode.ReadOnly);
            Session["corr_type_val"] = null;
            Session["sheet_calc"] = null;
            Session["auto_calc"] = null;
        }
    }
    protected void btn_update_cancel(object sender, EventArgs e)
    {
        Session["sheet_calc"] = null;
        Session["auto_calc"] = null;
    }
    protected void FormView_Layout_DataBound(object sender, EventArgs e)
    {
        if (FormView_Layout.CurrentMode == FormViewMode.ReadOnly)
        {
            UserClass UserLogin = (UserClass)Session["User"];
            Button job = (Button)FormView_Layout.FindControl("jobButton");
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "corr_layout.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;


                func1 f1 = new func1();
                //Response.Write(Page);
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                try
                {
                    if (aUsers == "external")
                    {
                        job.Visible = false;
                    }
                }
                catch { }
            }
        }
        if (FormView_Layout.CurrentMode == FormViewMode.Edit)
        {
            if (Session["auto_calc"] != null)
            {
                TextBox gwid = (TextBox)FormView_Layout.FindControl("vGrosShetWidTextBox");
                TextBox glen = (TextBox)FormView_Layout.FindControl("vGrosShetLenTextBox");
                TextBox netwid = (TextBox)FormView_Layout.FindControl("vNetShetWidTextBox");
                TextBox netlen = (TextBox)FormView_Layout.FindControl("vNetShetLenTextBox");
                TextBox diewid = (TextBox)FormView_Layout.FindControl("vDieSizeWidTextBox");
                TextBox dielen = (TextBox)FormView_Layout.FindControl("vDieSizeLenTextBox");
                gwid.Enabled = false;
                glen.Enabled = false;
                netlen.Enabled = false;
                netwid.Enabled = false;
                dielen.Enabled = false;
                diewid.Enabled = false;

                gwid.BackColor = System.Drawing.Color.Turquoise;
                glen.BackColor = System.Drawing.Color.Turquoise;
                netwid.BackColor = System.Drawing.Color.Turquoise;
                netlen.BackColor = System.Drawing.Color.Turquoise;
                diewid.BackColor = System.Drawing.Color.Turquoise;
                dielen.BackColor = System.Drawing.Color.Turquoise;

                TextBox machine = (TextBox)FormView_Layout.FindControl("vMachineTextBox");
                Label machineDesc = (Label)FormView_Layout.FindControl("vMachDscrTextBox");
                TextBox frontback = (TextBox)FormView_Layout.FindControl("vFrontBackTextBox");
                TextBox sideside = (TextBox)FormView_Layout.FindControl("vSideSideTextBox");
                DropDownList revcorr = (DropDownList)FormView_Layout.FindControl("vRevCorrDropDown");
                TextBox board = (TextBox)FormView_Layout.FindControl("vBoardTextBox");
                TextBox boardname = (TextBox)FormView_Layout.FindControl("vBoardNameTextBox");
                TextBox costuom = (TextBox)FormView_Layout.FindControl("vCostUomTextBox");
                TextBox costmsf = (TextBox)FormView_Layout.FindControl("vCostMsfTextBox");
                TextBox freightuom = (TextBox)FormView_Layout.FindControl("vFreightUomTextBox");
                TextBox freightmsf = (TextBox)FormView_Layout.FindControl("vFreightCwtTextBox");
                //TextBox nc = (TextBox)FormView_Layout.FindControl("vNcTextBox");
                DropDownList dnc = (DropDownList)FormView_Layout.FindControl("ddl_nc");
                TextBox grosshetwid = (TextBox)FormView_Layout.FindControl("vGrosShetWidTextBox");
                TextBox grosshetlen = (TextBox)FormView_Layout.FindControl("vGrosShetLenTextBox");
                TextBox outwid = (TextBox)FormView_Layout.FindControl("vOutWidTextBox");
                TextBox outlen = (TextBox)FormView_Layout.FindControl("vOutLenTextBox");
                TextBox outcut = (TextBox)FormView_Layout.FindControl("vOutCutTextBox");
                TextBox dieinches = (TextBox)FormView_Layout.FindControl("vDieInchesTextBox");
                TextBox netshetwid = (TextBox)FormView_Layout.FindControl("vNetShetWidTextBox");
                TextBox netshetlen = (TextBox)FormView_Layout.FindControl("vNetShetLenTextBox");
                TextBox diesizewid = (TextBox)FormView_Layout.FindControl("vDieSizeWidTextBox");
                TextBox diesizelen = (TextBox)FormView_Layout.FindControl("vDieSizeLenTextBox");
                TextBox onwid = (TextBox)FormView_Layout.FindControl("vOnWidTextBox");
                TextBox onlen = (TextBox)FormView_Layout.FindControl("vOnLenTextBox");
                Label ontotalup = (Label)FormView_Layout.FindControl("vOnTotalUpLabel");
                Label style = (Label)FormView_Layout.FindControl("styleLabel");

                try
                {


                    UserClass.CheckLogin(Page);
                    UserClass UserLogin = (UserClass)Session["User"];
                    Corrugated ord = new Corrugated();
                    DataSet ds = new DataSet();
                    ds = ord.CorrLayHandMach("Machine", UserLogin.UserName, "", "", "", Convert.ToString(Session["order_corrugated_est"]), Convert.ToInt32(Session["order_corrugated_formno"]), Convert.ToInt32(Session["order_corrugated_blankno"]), board.Text.Trim(), style.Text.Trim(), machine.Text.Trim(), revcorr.SelectedValue);

                    sideside.Text = ds.Tables[0].Rows[0][0].ToString();
                    frontback.Text = ds.Tables[0].Rows[0][1].ToString();
                    grosshetlen.Text = ds.Tables[0].Rows[0][2].ToString();
                    grosshetwid.Text = ds.Tables[0].Rows[0][3].ToString();
                    netshetlen.Text = ds.Tables[0].Rows[0][4].ToString();
                    netshetwid.Text = ds.Tables[0].Rows[0][5].ToString();
                    diesizelen.Text = ds.Tables[0].Rows[0][6].ToString();
                    diesizewid.Text = ds.Tables[0].Rows[0][7].ToString();
                    outwid.Text = ds.Tables[0].Rows[0][8].ToString();
                    outlen.Text = ds.Tables[0].Rows[0][9].ToString();
                    outcut.Text = ds.Tables[0].Rows[0][10].ToString();
                    onwid.Text = ds.Tables[0].Rows[0][15].ToString();
                    onlen.Text = ds.Tables[0].Rows[0][16].ToString();
                    ontotalup.Text = ds.Tables[0].Rows[0][17].ToString();
                    machineDesc.Text = ds.Tables[0].Rows[0][20].ToString();
                    dieinches.Text = Convert.ToString(Math.Round(Convert.ToDecimal(ds.Tables[0].Rows[0][21].ToString())));

                }
                catch { }
               
            }
            if (Session["sheet_calc"] != null)
            {
                TextBox waxlabel1 = (TextBox)FormView_Layout.FindControl("vWaxLabel1TextBox");
                TextBox waxdesc1 = (TextBox)FormView_Layout.FindControl("vWaxDesc1TextBox");
                TextBox b1 = (TextBox)FormView_Layout.FindControl("vLeafB1TextBox");
                TextBox leafwid1 = (TextBox)FormView_Layout.FindControl("vLeafWid1TextBox");
                TextBox leaflen1 = (TextBox)FormView_Layout.FindControl("vLeafLen1TextBox");
                TextBox waxlabel2 = (TextBox)FormView_Layout.FindControl("vWaxLabel2TextBox");
                TextBox waxdesc2 = (TextBox)FormView_Layout.FindControl("vWaxDesc2TextBox");
                TextBox b2 = (TextBox)FormView_Layout.FindControl("vLeafB2TextBox");
                TextBox leafwid2 = (TextBox)FormView_Layout.FindControl("vLeafWid2TextBox");
                TextBox leaflen2 = (TextBox)FormView_Layout.FindControl("vLeafLen2TextBox");
                TextBox waxlabel3 = (TextBox)FormView_Layout.FindControl("vWaxLabel3TextBox");
                TextBox waxdesc3 = (TextBox)FormView_Layout.FindControl("vWaxDesc3TextBox");
                TextBox b3 = (TextBox)FormView_Layout.FindControl("vLeafB3TextBox");
                TextBox leafwid3 = (TextBox)FormView_Layout.FindControl("vLeafWid3TextBox");
                TextBox leaflen3 = (TextBox)FormView_Layout.FindControl("vLeafLen3TextBox");
                TextBox waxlabel4 = (TextBox)FormView_Layout.FindControl("vWaxLabel4TextBox");
                TextBox waxdesc4 = (TextBox)FormView_Layout.FindControl("vWaxDesc4TextBox");
                TextBox b4 = (TextBox)FormView_Layout.FindControl("vLeafB4TextBox");
                TextBox leafwid4 = (TextBox)FormView_Layout.FindControl("vLeafWid4TextBox");
                TextBox leaflen4 = (TextBox)FormView_Layout.FindControl("vLeafLen4TextBox");

                TextBox frontback = (TextBox)FormView_Layout.FindControl("vFrontBackTextBox");
                TextBox side = (TextBox)FormView_Layout.FindControl("vSideSideTextBox");
                TextBox board = (TextBox)FormView_Layout.FindControl("vBoardTextBox");
                TextBox brddesc = (TextBox)FormView_Layout.FindControl("vBoardNameTextBox");
                TextBox costuom = (TextBox)FormView_Layout.FindControl("vCostUomTextBox");
                TextBox costmsf = (TextBox)FormView_Layout.FindControl("vCostMsfTextBox");
                TextBox freightuom = (TextBox)FormView_Layout.FindControl("vFreightUomTextBox");
                TextBox freightcwt = (TextBox)FormView_Layout.FindControl("vFreightCwtTextBox");
                TextBox gwid = (TextBox)FormView_Layout.FindControl("vGrosShetWidTextBox");
                TextBox glen = (TextBox)FormView_Layout.FindControl("vGrosShetLenTextBox");
                TextBox outlen = (TextBox)FormView_Layout.FindControl("vOutLenTextBox");
                TextBox outwid = (TextBox)FormView_Layout.FindControl("vOutWidTextBox");
                TextBox outcut = (TextBox)FormView_Layout.FindControl("vOutCutTextBox");
                TextBox dieinch = (TextBox)FormView_Layout.FindControl("vDieInchesTextBox");
                TextBox netwid = (TextBox)FormView_Layout.FindControl("vNetShetWidTextBox");
                TextBox netlen = (TextBox)FormView_Layout.FindControl("vNetShetLenTextBox");
                TextBox diewid = (TextBox)FormView_Layout.FindControl("vDieSizeWidTextBox");
                TextBox dielen = (TextBox)FormView_Layout.FindControl("vDieSizeLenTextBox");
                TextBox onlen = (TextBox)FormView_Layout.FindControl("vOnLenTextBox");
                TextBox onwid = (TextBox)FormView_Layout.FindControl("vOnWidTextBox");
                TextBox adder1 = (TextBox)FormView_Layout.FindControl("vAdders1TextBox");
                TextBox adder2 = (TextBox)FormView_Layout.FindControl("vAdders2TextBox");
                TextBox adder3 = (TextBox)FormView_Layout.FindControl("vAdders3TextBox");
                TextBox adder4 = (TextBox)FormView_Layout.FindControl("vAdders4TextBox");
                TextBox adder5 = (TextBox)FormView_Layout.FindControl("vAdders5TextBox");
                TextBox adder6 = (TextBox)FormView_Layout.FindControl("vAdders6TextBox");
                TextBox adder7 = (TextBox)FormView_Layout.FindControl("vAdders7TextBox");
                TextBox adder8 = (TextBox)FormView_Layout.FindControl("vAdders8TextBox");
                TextBox adder9 = (TextBox)FormView_Layout.FindControl("vAdders9TextBox");
                TextBox adder10 = (TextBox)FormView_Layout.FindControl("vAdders10TextBox");
                TextBox adder11 = (TextBox)FormView_Layout.FindControl("vAdders11TextBox");
                TextBox adder12 = (TextBox)FormView_Layout.FindControl("vAdders12TextBox");
                //TextBox leafwid4 = (TextBox)FormView_Layout.FindControl("vLeafWid4TextBox");
                DropDownList revcor = (DropDownList)FormView_Layout.FindControl("vRevCorrDropDown");
                DropDownList nc = (DropDownList)FormView_Layout.FindControl("ddl_nc");
                Label sheetcal = (Label)FormView_Layout.FindControl("sheetLabel");
                sheetcal.Text = "sheet";

                Image img1 = (Image)FormView_Layout.FindControl("Image7");
                Image img2 = (Image)FormView_Layout.FindControl("Image8");
                Image img3 = (Image)FormView_Layout.FindControl("Image9");
                Image img4 = (Image)FormView_Layout.FindControl("Image10");
                Image img5 = (Image)FormView_Layout.FindControl("img_board");
                Image img6 = (Image)FormView_Layout.FindControl("Image1");
                Image img7 = (Image)FormView_Layout.FindControl("Image2");
                Image img8 = (Image)FormView_Layout.FindControl("Image3");
                Image img9 = (Image)FormView_Layout.FindControl("Image4");
                Image img10 = (Image)FormView_Layout.FindControl("Image5");
                Image img11 = (Image)FormView_Layout.FindControl("Image6");

                img1.Visible = false;
                img2.Visible = false;
                img3.Visible = false;
                img4.Visible = false;
                img5.Visible = false;
                img6.Visible = false;
                img7.Visible = false;
                img8.Visible = false;
                img9.Visible = false;
                img10.Visible = false;
                img11.Visible = false;


                frontback.Enabled = false;
                side.Enabled = false;
                board.Enabled = false;
                brddesc.Enabled = false;
                costmsf.Enabled = false;
                costuom.Enabled = false;
                freightcwt.Enabled = false;
                freightuom.Enabled = false;
                gwid.Enabled = false;
                glen.Enabled = false;
                netlen.Enabled = false;
                netwid.Enabled = false;
                outlen.Enabled = false;
                outwid.Enabled = false;
                outcut.Enabled = false;
                dieinch.Enabled = false;
                dielen.Enabled = false;
                diewid.Enabled = false;
                onlen.Enabled = false;
                onwid.Enabled = false;
                adder1.Enabled = false;
                adder2.Enabled = false;
                adder3.Enabled = false;
                adder4.Enabled = false;
                adder5.Enabled = false;
                adder6.Enabled = false;
                adder7.Enabled = false;
                adder8.Enabled = false;
                adder9.Enabled = false;
                adder10.Enabled = false;
                adder11.Enabled = false;
                adder12.Enabled = false;
                revcor.Enabled = false;
                nc.Enabled = false;

                frontback.BackColor = System.Drawing.Color.Turquoise;
                side.BackColor = System.Drawing.Color.Turquoise;
                board.BackColor = System.Drawing.Color.Turquoise;
                brddesc.BackColor = System.Drawing.Color.Turquoise;
                costmsf.BackColor = System.Drawing.Color.Turquoise;
                costuom.BackColor = System.Drawing.Color.Turquoise;
                freightcwt.BackColor = System.Drawing.Color.Turquoise;
                freightuom.BackColor = System.Drawing.Color.Turquoise;
                gwid.BackColor = System.Drawing.Color.Turquoise;
                glen.BackColor = System.Drawing.Color.Turquoise;
                netwid.BackColor = System.Drawing.Color.Turquoise;
                netlen.BackColor = System.Drawing.Color.Turquoise;
                outlen.BackColor = System.Drawing.Color.Turquoise;
                outwid.BackColor = System.Drawing.Color.Turquoise;
                outcut.BackColor = System.Drawing.Color.Turquoise;
                dieinch.BackColor = System.Drawing.Color.Turquoise;
                dielen.BackColor = System.Drawing.Color.Turquoise;
                diewid.BackColor = System.Drawing.Color.Turquoise;
                onlen.BackColor = System.Drawing.Color.Turquoise;
                onwid.BackColor = System.Drawing.Color.Turquoise;
                adder1.BackColor = System.Drawing.Color.Turquoise;
                adder2.BackColor = System.Drawing.Color.Turquoise;
                adder3.BackColor = System.Drawing.Color.Turquoise;
                adder4.BackColor = System.Drawing.Color.Turquoise;
                adder5.BackColor = System.Drawing.Color.Turquoise;
                adder6.BackColor = System.Drawing.Color.Turquoise;
                adder7.BackColor = System.Drawing.Color.Turquoise;
                adder8.BackColor = System.Drawing.Color.Turquoise;
                adder9.BackColor = System.Drawing.Color.Turquoise;
                adder10.BackColor = System.Drawing.Color.Turquoise;
                adder11.BackColor = System.Drawing.Color.Turquoise;
                adder12.BackColor = System.Drawing.Color.Turquoise;
                revcor.BackColor = System.Drawing.Color.Turquoise;
                nc.BackColor = System.Drawing.Color.Turquoise;

                waxlabel1.Enabled = false;
                waxlabel2.Enabled = false;
                waxlabel3.Enabled = false;
                waxlabel4.Enabled = false;
                waxdesc1.Enabled = false;
                waxdesc2.Enabled = false;
                waxdesc3.Enabled = false;
                waxdesc4.Enabled = false;
                b1.Enabled = false;
                b2.Enabled = false;
                b3.Enabled = false;
                b4.Enabled = false;
                leaflen1.Enabled = false;
                leaflen2.Enabled = false;
                leaflen3.Enabled = false;
                leaflen4.ReadOnly = true;
                leafwid1.Enabled = false;
                leafwid2.Enabled = false;
                leafwid3.Enabled = false;
                leafwid4.Enabled = false;
                waxdesc1.BackColor = System.Drawing.Color.Turquoise;
                waxdesc2.BackColor = System.Drawing.Color.Turquoise;
                waxdesc3.BackColor = System.Drawing.Color.Turquoise;
                waxdesc4.BackColor = System.Drawing.Color.Turquoise;
                waxlabel1.BackColor = System.Drawing.Color.Turquoise;
                waxlabel2.BackColor = System.Drawing.Color.Turquoise;
                waxlabel3.BackColor = System.Drawing.Color.Turquoise;
                waxlabel4.BackColor = System.Drawing.Color.Turquoise;
                b1.BackColor = System.Drawing.Color.Turquoise;
                b2.BackColor = System.Drawing.Color.Turquoise;
                b3.BackColor = System.Drawing.Color.Turquoise;
                b4.BackColor = System.Drawing.Color.Turquoise;
                leaflen1.BackColor = System.Drawing.Color.Turquoise;
                leaflen2.BackColor = System.Drawing.Color.Turquoise;
                leaflen3.BackColor = System.Drawing.Color.Turquoise;
                leaflen4.BackColor = System.Drawing.Color.Turquoise;
                leafwid1.BackColor = System.Drawing.Color.Turquoise;
                leafwid2.BackColor = System.Drawing.Color.Turquoise;
                leafwid3.BackColor = System.Drawing.Color.Turquoise;
                leafwid4.BackColor = System.Drawing.Color.Turquoise;
            }
            if (Session["sheet_calc"] == null && Session["auto_calc"] == null)
            {
                TextBox machine = (TextBox)FormView_Layout.FindControl("vMachineTextBox");
                Image maclook = (Image)FormView_Layout.FindControl("Image11");
                
                maclook.Visible = false;
                machine.Enabled = false;
                machine.BackColor = System.Drawing.Color.Turquoise;
                
            }
            if (Convert.ToInt32(Session["corr_type_val"]) == 8)
            {
                TextBox waxlabel1 = (TextBox)FormView_Layout.FindControl("vWaxLabel1TextBox");
                TextBox waxdesc1 = (TextBox)FormView_Layout.FindControl("vWaxDesc1TextBox");
                // TextBox s1 = (TextBox)FormView_Layout.FindControl("vLeafS1TextBox");
                TextBox b1 = (TextBox)FormView_Layout.FindControl("vLeafB1TextBox");
                TextBox leafwid1 = (TextBox)FormView_Layout.FindControl("vLeafWid1TextBox");
                TextBox leaflen1 = (TextBox)FormView_Layout.FindControl("vLeafLen1TextBox");
                TextBox waxlabel2 = (TextBox)FormView_Layout.FindControl("vWaxLabel2TextBox");
                TextBox waxdesc2 = (TextBox)FormView_Layout.FindControl("vWaxDesc2TextBox");
                // TextBox s2 = (TextBox)FormView_Layout.FindControl("vLeafS2TextBox");
                TextBox b2 = (TextBox)FormView_Layout.FindControl("vLeafB2TextBox");
                TextBox leafwid2 = (TextBox)FormView_Layout.FindControl("vLeafWid2TextBox");
                TextBox leaflen2 = (TextBox)FormView_Layout.FindControl("vLeafLen2TextBox");
                TextBox waxlabel3 = (TextBox)FormView_Layout.FindControl("vWaxLabel3TextBox");
                TextBox waxdesc3 = (TextBox)FormView_Layout.FindControl("vWaxDesc3TextBox");
                // TextBox s3 = (TextBox)FormView_Layout.FindControl("vLeafS3TextBox");
                TextBox b3 = (TextBox)FormView_Layout.FindControl("vLeafB3TextBox");
                TextBox leafwid3 = (TextBox)FormView_Layout.FindControl("vLeafWid3TextBox");
                TextBox leaflen3 = (TextBox)FormView_Layout.FindControl("vLeafLen3TextBox");
                TextBox waxlabel4 = (TextBox)FormView_Layout.FindControl("vWaxLabel4TextBox");
                TextBox waxdesc4 = (TextBox)FormView_Layout.FindControl("vWaxDesc4TextBox");
                // TextBox s4 = (TextBox)FormView_Layout.FindControl("vLeafS4TextBox");
                TextBox b4 = (TextBox)FormView_Layout.FindControl("vLeafB4TextBox");
                TextBox leafwid4 = (TextBox)FormView_Layout.FindControl("vLeafWid4TextBox");
                TextBox leaflen4 = (TextBox)FormView_Layout.FindControl("vLeafLen4TextBox");
                Image img1 = (Image)FormView_Layout.FindControl("Image7");
                Image img2 = (Image)FormView_Layout.FindControl("Image8");
                Image img3 = (Image)FormView_Layout.FindControl("Image9");
                Image img4 = (Image)FormView_Layout.FindControl("Image10");

                img1.Visible = false;
                img2.Visible = false;
                img3.Visible = false;
                img4.Visible = false;

                waxlabel1.Enabled = false;
                waxlabel2.Enabled = false;
                waxlabel3.Enabled = false;
                waxlabel4.Enabled = false;
                waxdesc1.Enabled = false;
                waxdesc2.Enabled = false;
                waxdesc3.Enabled = false;
                waxdesc4.Enabled = false;
                b1.Enabled = false;
                b2.Enabled = false;
                b3.Enabled = false;
                b4.Enabled = false;
                leaflen1.Enabled = false;
                leaflen2.Enabled = false;
                leaflen3.Enabled = false;
                leaflen4.Enabled = false;
                leafwid1.Enabled = false;
                leafwid2.Enabled = false;
                leafwid3.Enabled = false;
                leafwid4.Enabled = false;
                waxdesc1.BackColor = System.Drawing.Color.Turquoise;
                waxdesc2.BackColor = System.Drawing.Color.Turquoise;
                waxdesc3.BackColor = System.Drawing.Color.Turquoise;
                waxdesc4.BackColor = System.Drawing.Color.Turquoise;
                waxlabel1.BackColor = System.Drawing.Color.Turquoise;
                waxlabel2.BackColor = System.Drawing.Color.Turquoise;
                waxlabel3.BackColor = System.Drawing.Color.Turquoise;
                waxlabel4.BackColor = System.Drawing.Color.Turquoise;
                b1.BackColor = System.Drawing.Color.Turquoise;
                b2.BackColor = System.Drawing.Color.Turquoise;
                b3.BackColor = System.Drawing.Color.Turquoise;
                b4.BackColor = System.Drawing.Color.Turquoise;
                leaflen1.BackColor = System.Drawing.Color.Turquoise;
                leaflen2.BackColor = System.Drawing.Color.Turquoise;
                leaflen3.BackColor = System.Drawing.Color.Turquoise;
                leaflen4.BackColor = System.Drawing.Color.Turquoise;
                leafwid1.BackColor = System.Drawing.Color.Turquoise;
                leafwid2.BackColor = System.Drawing.Color.Turquoise;
                leafwid3.BackColor = System.Drawing.Color.Turquoise;
                leafwid4.BackColor = System.Drawing.Color.Turquoise;

            }
        }
    }

    protected void btn_sheet_calc_click(object sender, EventArgs e)
    {
        Session["sheet_calc"] = 1;
    }
    protected void btn_auto_calc_click(object sender, EventArgs e)
    {
        Session["auto_calc"] = 1;
    }

    protected void machine_textchange(object sender, EventArgs e)
    {
        TextBox machine = (TextBox)FormView_Layout.FindControl("vMachineTextBox");
        Label machineDesc = (Label)FormView_Layout.FindControl("vMachDscrTextBox");
        TextBox frontback = (TextBox)FormView_Layout.FindControl("vFrontBackTextBox");
        TextBox sideside = (TextBox)FormView_Layout.FindControl("vSideSideTextBox");
        DropDownList revcorr = (DropDownList)FormView_Layout.FindControl("vRevCorrDropDown");
        TextBox board = (TextBox)FormView_Layout.FindControl("vBoardTextBox");
        TextBox boardname = (TextBox)FormView_Layout.FindControl("vBoardNameTextBox");
        TextBox costuom = (TextBox)FormView_Layout.FindControl("vCostUomTextBox");
        TextBox costmsf = (TextBox)FormView_Layout.FindControl("vCostMsfTextBox");
        TextBox freightuom = (TextBox)FormView_Layout.FindControl("vFreightUomTextBox");
        TextBox freightmsf = (TextBox)FormView_Layout.FindControl("vFreightCwtTextBox");
        //TextBox nc = (TextBox)FormView_Layout.FindControl("vNcTextBox");
        DropDownList dnc = (DropDownList)FormView_Layout.FindControl("ddl_nc");
        TextBox grosshetwid = (TextBox)FormView_Layout.FindControl("vGrosShetWidTextBox");
        TextBox grosshetlen = (TextBox)FormView_Layout.FindControl("vGrosShetLenTextBox");
        TextBox outwid = (TextBox)FormView_Layout.FindControl("vOutWidTextBox");
        TextBox outlen = (TextBox)FormView_Layout.FindControl("vOutLenTextBox");
        TextBox outcut = (TextBox)FormView_Layout.FindControl("vOutCutTextBox");
        TextBox dieinches = (TextBox)FormView_Layout.FindControl("vDieInchesTextBox");
        TextBox netshetwid = (TextBox)FormView_Layout.FindControl("vNetShetWidTextBox");
        TextBox netshetlen = (TextBox)FormView_Layout.FindControl("vNetShetLenTextBox");
        TextBox diesizewid = (TextBox)FormView_Layout.FindControl("vDieSizeWidTextBox");
        TextBox diesizelen = (TextBox)FormView_Layout.FindControl("vDieSizeLenTextBox");
        TextBox onwid = (TextBox)FormView_Layout.FindControl("vOnWidTextBox");
        TextBox onlen = (TextBox)FormView_Layout.FindControl("vOnLenTextBox");
        Label ontotalup = (Label)FormView_Layout.FindControl("vOnTotalUpLabel");
        Label style = (Label)FormView_Layout.FindControl("styleLabel");
        try
        {

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Corrugated ord = new Corrugated();
            DataSet ds = new DataSet();
            ds = ord.CorrLayHandMach("Machine", UserLogin.UserName, "", "", "", Convert.ToString(Session["order_corrugated_est"]), Convert.ToInt32(Session["order_corrugated_formno"]), Convert.ToInt32(Session["order_corrugated_blankno"]), board.Text.Trim(), style.Text.Trim(), machine.Text.Trim(), revcorr.SelectedValue);
            
            sideside.Text = ds.Tables[0].Rows[0][0].ToString();
            frontback.Text = ds.Tables[0].Rows[0][1].ToString();
            grosshetlen.Text = ds.Tables[0].Rows[0][2].ToString();
            grosshetwid.Text = ds.Tables[0].Rows[0][3].ToString();
            netshetlen.Text = ds.Tables[0].Rows[0][4].ToString();
            netshetwid.Text = ds.Tables[0].Rows[0][5].ToString();
            diesizelen.Text = ds.Tables[0].Rows[0][6].ToString();
            diesizewid.Text = ds.Tables[0].Rows[0][7].ToString();
            outwid.Text = ds.Tables[0].Rows[0][8].ToString();
            outlen.Text = ds.Tables[0].Rows[0][9].ToString();
            outcut.Text = ds.Tables[0].Rows[0][10].ToString();
            onwid.Text = ds.Tables[0].Rows[0][15].ToString();
            onlen.Text = ds.Tables[0].Rows[0][16].ToString();
            ontotalup.Text = ds.Tables[0].Rows[0][17].ToString();
            machineDesc.Text = ds.Tables[0].Rows[0][20].ToString();
            dieinches.Text = Convert.ToString(Math.Round(Convert.ToDecimal(ds.Tables[0].Rows[0][21].ToString())));
            frontback.Focus();
        }
        catch { }
        
    }

    protected void Board_textchange_click(object sender, EventArgs e)
    {
        TextBox machine = (TextBox)FormView_Layout.FindControl("vMachineTextBox");
        Label machineDesc = (Label)FormView_Layout.FindControl("vMachDscrTextBox");
        TextBox frontback = (TextBox)FormView_Layout.FindControl("vFrontBackTextBox");
        TextBox sideside = (TextBox)FormView_Layout.FindControl("vSideSideTextBox");
        DropDownList revcorr = (DropDownList)FormView_Layout.FindControl("vRevCorrDropDown");
        TextBox board = (TextBox)FormView_Layout.FindControl("vBoardTextBox");
        TextBox boardname = (TextBox)FormView_Layout.FindControl("vBoardNameTextBox");
        TextBox costuom = (TextBox)FormView_Layout.FindControl("vCostUomTextBox");
        TextBox costmsf = (TextBox)FormView_Layout.FindControl("vCostMsfTextBox");
        TextBox freightuom = (TextBox)FormView_Layout.FindControl("vFreightUomTextBox");
        TextBox freightmsf = (TextBox)FormView_Layout.FindControl("vFreightCwtTextBox");
        //TextBox nc = (TextBox)FormView_Layout.FindControl("vNcTextBox");
        DropDownList dnc = (DropDownList)FormView_Layout.FindControl("ddl_nc");
        TextBox grosshetwid = (TextBox)FormView_Layout.FindControl("vGrosShetWidTextBox");
        TextBox grosshetlen = (TextBox)FormView_Layout.FindControl("vGrosShetLenTextBox");
        TextBox outwid = (TextBox)FormView_Layout.FindControl("vOutWidTextBox");
        TextBox outlen = (TextBox)FormView_Layout.FindControl("vOutLenTextBox");
        TextBox outcut = (TextBox)FormView_Layout.FindControl("vOutCutTextBox");
        TextBox dieinches = (TextBox)FormView_Layout.FindControl("vDieInchesTextBox");
        TextBox netshetwid = (TextBox)FormView_Layout.FindControl("vNetShetWidTextBox");
        TextBox netshetlen = (TextBox)FormView_Layout.FindControl("vNetShetLenTextBox");
        TextBox diesizewid = (TextBox)FormView_Layout.FindControl("vDieSizeWidTextBox");
        TextBox diesizelen = (TextBox)FormView_Layout.FindControl("vDieSizeLenTextBox");
        TextBox onwid = (TextBox)FormView_Layout.FindControl("vOnWidTextBox");
        TextBox onlen = (TextBox)FormView_Layout.FindControl("vOnLenTextBox");
        Label ontotalup = (Label)FormView_Layout.FindControl("vOnTotalUpLabel");
        Label style = (Label)FormView_Layout.FindControl("styleLabel");
        Label flute = (Label)FormView_Layout.FindControl("vFluteLabel");
        Label test = (Label)FormView_Layout.FindControl("vTestLabel");
        try
        {
        

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Corrugated ord = new Corrugated();
            DataSet ds = new DataSet();
            ds = ord.CorrLayHandMach("Machine", UserLogin.UserName, "", "", "", Convert.ToString(Session["order_corrugated_est"]), Convert.ToInt32(Session["order_corrugated_formno"]), Convert.ToInt32(Session["order_corrugated_blankno"]), board.Text.Trim(), style.Text.Trim(), machine.Text.Trim(), revcorr.SelectedValue);

            
            
            sideside.Text = ds.Tables[0].Rows[0][0].ToString();
            frontback.Text = ds.Tables[0].Rows[0][1].ToString();
            grosshetlen.Text = ds.Tables[0].Rows[0][2].ToString();
            grosshetwid.Text = ds.Tables[0].Rows[0][3].ToString();
            netshetlen.Text = ds.Tables[0].Rows[0][4].ToString();
            netshetwid.Text = ds.Tables[0].Rows[0][5].ToString();
            diesizelen.Text = ds.Tables[0].Rows[0][6].ToString();
            diesizewid.Text = ds.Tables[0].Rows[0][7].ToString();
            outwid.Text = ds.Tables[0].Rows[0][8].ToString();
            outlen.Text = ds.Tables[0].Rows[0][9].ToString();
            outcut.Text = ds.Tables[0].Rows[0][10].ToString();
            onwid.Text = ds.Tables[0].Rows[0][15].ToString();
            onlen.Text = ds.Tables[0].Rows[0][16].ToString();
            ontotalup.Text = ds.Tables[0].Rows[0][17].ToString();
            machineDesc.Text= ds.Tables[0].Rows[0][20].ToString();
            flute.Text = ds.Tables[0].Rows[0][18].ToString();
            test.Text = ds.Tables[0].Rows[0][19].ToString();
            dieinches.Text = Convert.ToString(Math.Round(Convert.ToDecimal(ds.Tables[0].Rows[0][21].ToString())));
            boardname.Focus();

        }
        catch { }
    }

    protected void OnLen_Text_Change(object sender, EventArgs e)
    {
        
        TextBox machine = (TextBox)FormView_Layout.FindControl("vMachineTextBox");
        Label machineDesc = (Label)FormView_Layout.FindControl("vMachDscrTextBox");
        TextBox frontback = (TextBox)FormView_Layout.FindControl("vFrontBackTextBox");
        TextBox sideside = (TextBox)FormView_Layout.FindControl("vSideSideTextBox");
        DropDownList revcorr = (DropDownList)FormView_Layout.FindControl("vRevCorrDropDown");
        TextBox board = (TextBox)FormView_Layout.FindControl("vBoardTextBox");
        TextBox boardname = (TextBox)FormView_Layout.FindControl("vBoardNameTextBox");
        TextBox costuom = (TextBox)FormView_Layout.FindControl("vCostUomTextBox");
        TextBox costmsf = (TextBox)FormView_Layout.FindControl("vCostMsfTextBox");
        TextBox freightuom = (TextBox)FormView_Layout.FindControl("vFreightUomTextBox");
        TextBox freightmsf = (TextBox)FormView_Layout.FindControl("vFreightCwtTextBox");
        //TextBox nc = (TextBox)FormView_Layout.FindControl("vNcTextBox");
        DropDownList dnc = (DropDownList)FormView_Layout.FindControl("ddl_nc");
        TextBox grosshetwid = (TextBox)FormView_Layout.FindControl("vGrosShetWidTextBox");
        TextBox grosshetlen = (TextBox)FormView_Layout.FindControl("vGrosShetLenTextBox");
        TextBox outwid = (TextBox)FormView_Layout.FindControl("vOutWidTextBox");
        TextBox outlen = (TextBox)FormView_Layout.FindControl("vOutLenTextBox");
        TextBox outcut = (TextBox)FormView_Layout.FindControl("vOutCutTextBox");
        TextBox dieinches = (TextBox)FormView_Layout.FindControl("vDieInchesTextBox");
        TextBox netshetwid = (TextBox)FormView_Layout.FindControl("vNetShetWidTextBox");
        TextBox netshetlen = (TextBox)FormView_Layout.FindControl("vNetShetLenTextBox");
        TextBox diesizewid = (TextBox)FormView_Layout.FindControl("vDieSizeWidTextBox");
        TextBox diesizelen = (TextBox)FormView_Layout.FindControl("vDieSizeLenTextBox");
        TextBox onwid = (TextBox)FormView_Layout.FindControl("vOnWidTextBox");
        TextBox onlen = (TextBox)FormView_Layout.FindControl("vOnLenTextBox");
        Label ontotalup = (Label)FormView_Layout.FindControl("vOnTotalUpLabel");
        Label style = (Label)FormView_Layout.FindControl("styleLabel");

       
        try
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Corrugated ord = new Corrugated();
            DataSet ds = new DataSet();

            ds = ord.CorrLayoutLen("On-Len", UserLogin.UserName, "", "", "", Convert.ToString(Session["order_corrugated_est"]), machine.Text.Trim(), Convert.ToInt32(Session["order_corrugated_formno"]), Convert.ToInt32(Session["order_corrugated_blankno"]), board.Text.Trim(), Convert.ToString(revcorr.SelectedValue), Convert.ToDecimal(outwid.Text.Trim()), Convert.ToDecimal(outlen.Text.Trim()), Convert.ToDecimal(onlen.Text.Trim()), Convert.ToDecimal(onwid.Text.Trim()), Convert.ToInt32(ontotalup.Text.Trim()), Convert.ToDecimal(grosshetlen.Text.Trim()), Convert.ToDecimal(grosshetwid.Text.Trim()), 0, Convert.ToDecimal(netshetlen.Text.Trim()), Convert.ToDecimal(netshetwid.Text.Trim()), 0);
                    
            sideside.Text = ds.Tables[0].Rows[0][0].ToString();
            frontback.Text = ds.Tables[0].Rows[0][1].ToString();
            grosshetlen.Text = ds.Tables[0].Rows[0][2].ToString();
            grosshetwid.Text = ds.Tables[0].Rows[0][3].ToString();
            netshetlen.Text = ds.Tables[0].Rows[0][4].ToString();
            netshetwid.Text = ds.Tables[0].Rows[0][5].ToString();
            diesizelen.Text = ds.Tables[0].Rows[0][6].ToString();
            diesizewid.Text = ds.Tables[0].Rows[0][7].ToString();
            outwid.Text = ds.Tables[0].Rows[0][8].ToString();
            outlen.Text = ds.Tables[0].Rows[0][9].ToString();
            outcut.Text = ds.Tables[0].Rows[0][10].ToString();
            onwid.Text = ds.Tables[0].Rows[0][15].ToString();
            onlen.Text = ds.Tables[0].Rows[0][16].ToString();
            ontotalup.Text = ds.Tables[0].Rows[0][17].ToString();
            dieinches.Text = Convert.ToString(Math.Round(Convert.ToDecimal(ds.Tables[0].Rows[0][18].ToString())));
           
            onwid.Focus();

        }
        catch { }

    }
  
    protected void OnLen_Text2_Change(object sender, EventArgs e)
    {
        TextBox machine = (TextBox)FormView_Layout.FindControl("vMachineTextBox");
        Label machineDesc = (Label)FormView_Layout.FindControl("vMachDscrTextBox");
        TextBox frontback = (TextBox)FormView_Layout.FindControl("vFrontBackTextBox");
        TextBox sideside = (TextBox)FormView_Layout.FindControl("vSideSideTextBox");
        DropDownList revcorr = (DropDownList)FormView_Layout.FindControl("vRevCorrDropDown");
        TextBox board = (TextBox)FormView_Layout.FindControl("vBoardTextBox");
        TextBox boardname = (TextBox)FormView_Layout.FindControl("vBoardNameTextBox");
        TextBox costuom = (TextBox)FormView_Layout.FindControl("vCostUomTextBox");
        TextBox costmsf = (TextBox)FormView_Layout.FindControl("vCostMsfTextBox");
        TextBox freightuom = (TextBox)FormView_Layout.FindControl("vFreightUomTextBox");
        TextBox freightmsf = (TextBox)FormView_Layout.FindControl("vFreightCwtTextBox");
        //TextBox nc = (TextBox)FormView_Layout.FindControl("vNcTextBox");
        DropDownList dnc = (DropDownList)FormView_Layout.FindControl("ddl_nc");
        TextBox grosshetwid = (TextBox)FormView_Layout.FindControl("vGrosShetWidTextBox");
        TextBox grosshetlen = (TextBox)FormView_Layout.FindControl("vGrosShetLenTextBox");
        TextBox outwid = (TextBox)FormView_Layout.FindControl("vOutWidTextBox");
        TextBox outlen = (TextBox)FormView_Layout.FindControl("vOutLenTextBox");
        TextBox outcut = (TextBox)FormView_Layout.FindControl("vOutCutTextBox");
        TextBox dieinches = (TextBox)FormView_Layout.FindControl("vDieInchesTextBox");
        TextBox netshetwid = (TextBox)FormView_Layout.FindControl("vNetShetWidTextBox");
        TextBox netshetlen = (TextBox)FormView_Layout.FindControl("vNetShetLenTextBox");
        TextBox diesizewid = (TextBox)FormView_Layout.FindControl("vDieSizeWidTextBox");
        TextBox diesizelen = (TextBox)FormView_Layout.FindControl("vDieSizeLenTextBox");
        TextBox onwid = (TextBox)FormView_Layout.FindControl("vOnWidTextBox");
        TextBox onlen = (TextBox)FormView_Layout.FindControl("vOnLenTextBox");
        Label ontotalup = (Label)FormView_Layout.FindControl("vOnTotalUpLabel");
        Label style = (Label)FormView_Layout.FindControl("styleLabel");
        try
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Corrugated ord = new Corrugated();
            DataSet ds = new DataSet();

            ds = ord.CorrLayoutLen("On-Len", UserLogin.UserName, "", "", "", Convert.ToString(Session["order_corrugated_est"]), machine.Text.Trim(), Convert.ToInt32(Session["order_corrugated_formno"]), Convert.ToInt32(Session["order_corrugated_blankno"]), board.Text.Trim(), Convert.ToString(revcorr.SelectedValue), Convert.ToDecimal(outwid.Text.Trim()), Convert.ToDecimal(outlen.Text.Trim()), Convert.ToDecimal(onlen.Text.Trim()), Convert.ToDecimal(onwid.Text.Trim()), Convert.ToInt32(ontotalup.Text.Trim()), Convert.ToDecimal(grosshetlen.Text.Trim()), Convert.ToDecimal(grosshetwid.Text.Trim()), 0, Convert.ToDecimal(netshetlen.Text.Trim()), Convert.ToDecimal(netshetwid.Text.Trim()), 0);

            sideside.Text = ds.Tables[0].Rows[0][0].ToString();
            frontback.Text = ds.Tables[0].Rows[0][1].ToString();
            grosshetlen.Text = ds.Tables[0].Rows[0][2].ToString();
            grosshetwid.Text = ds.Tables[0].Rows[0][3].ToString();
            netshetlen.Text = ds.Tables[0].Rows[0][4].ToString();
            netshetwid.Text = ds.Tables[0].Rows[0][5].ToString();
            diesizelen.Text = ds.Tables[0].Rows[0][6].ToString();
            diesizewid.Text = ds.Tables[0].Rows[0][7].ToString();
            outwid.Text = ds.Tables[0].Rows[0][8].ToString();
            outlen.Text = ds.Tables[0].Rows[0][9].ToString();
            outcut.Text = ds.Tables[0].Rows[0][10].ToString();
            onwid.Text = ds.Tables[0].Rows[0][15].ToString();
            onlen.Text = ds.Tables[0].Rows[0][16].ToString();
            ontotalup.Text = ds.Tables[0].Rows[0][17].ToString();
            dieinches.Text = Convert.ToString(Math.Round(Convert.ToDecimal(ds.Tables[0].Rows[0][18].ToString())));
           
            outwid.Focus();
                        
        }
        catch { }



    }
   

    protected void OutLenchange_Click(object sender, EventArgs e)
    {

        TextBox machine = (TextBox)FormView_Layout.FindControl("vMachineTextBox");
        Label machineDesc = (Label)FormView_Layout.FindControl("vMachDscrTextBox");
        TextBox frontback = (TextBox)FormView_Layout.FindControl("vFrontBackTextBox");
        TextBox sideside = (TextBox)FormView_Layout.FindControl("vSideSideTextBox");
        DropDownList revcorr = (DropDownList)FormView_Layout.FindControl("vRevCorrDropDown");
        TextBox board = (TextBox)FormView_Layout.FindControl("vBoardTextBox");
        TextBox boardname = (TextBox)FormView_Layout.FindControl("vBoardNameTextBox");
        TextBox costuom = (TextBox)FormView_Layout.FindControl("vCostUomTextBox");
        TextBox costmsf = (TextBox)FormView_Layout.FindControl("vCostMsfTextBox");
        TextBox freightuom = (TextBox)FormView_Layout.FindControl("vFreightUomTextBox");
        TextBox freightmsf = (TextBox)FormView_Layout.FindControl("vFreightCwtTextBox");
        //TextBox nc = (TextBox)FormView_Layout.FindControl("vNcTextBox");
        DropDownList dnc = (DropDownList)FormView_Layout.FindControl("ddl_nc");
        TextBox grosshetwid = (TextBox)FormView_Layout.FindControl("vGrosShetWidTextBox");
        TextBox grosshetlen = (TextBox)FormView_Layout.FindControl("vGrosShetLenTextBox");
        TextBox outwid = (TextBox)FormView_Layout.FindControl("vOutWidTextBox");
        TextBox outlen = (TextBox)FormView_Layout.FindControl("vOutLenTextBox");
        TextBox outcut = (TextBox)FormView_Layout.FindControl("vOutCutTextBox");
        TextBox dieinches = (TextBox)FormView_Layout.FindControl("vDieInchesTextBox");
        TextBox netshetwid = (TextBox)FormView_Layout.FindControl("vNetShetWidTextBox");
        TextBox netshetlen = (TextBox)FormView_Layout.FindControl("vNetShetLenTextBox");
        TextBox diesizewid = (TextBox)FormView_Layout.FindControl("vDieSizeWidTextBox");
        TextBox diesizelen = (TextBox)FormView_Layout.FindControl("vDieSizeLenTextBox");
        TextBox onwid = (TextBox)FormView_Layout.FindControl("vOnWidTextBox");
        TextBox onlen = (TextBox)FormView_Layout.FindControl("vOnLenTextBox");
        Label ontotalup = (Label)FormView_Layout.FindControl("vOnTotalUpLabel");
        Label style = (Label)FormView_Layout.FindControl("styleLabel");
        try
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Corrugated ord = new Corrugated();
            DataSet ds = new DataSet();

            ds = ord.CorrLayoutLen("OutLen", UserLogin.UserName, "", "", "", Convert.ToString(Session["order_corrugated_est"]), machine.Text.Trim(), Convert.ToInt32(Session["order_corrugated_formno"]), Convert.ToInt32(Session["order_corrugated_blankno"]), board.Text.Trim(), Convert.ToString(revcorr.SelectedValue), Convert.ToDecimal(outwid.Text.Trim()), Convert.ToDecimal(outlen.Text.Trim()), Convert.ToDecimal(onlen.Text.Trim()), Convert.ToDecimal(onwid.Text.Trim()), Convert.ToInt32(ontotalup.Text.Trim()), Convert.ToDecimal(grosshetlen.Text.Trim()), Convert.ToDecimal(grosshetwid.Text.Trim()), 0, Convert.ToDecimal(netshetlen.Text.Trim()), Convert.ToDecimal(netshetwid.Text.Trim()), 0);

          
            grosshetlen.Text = ds.Tables[0].Rows[0][2].ToString();
            grosshetwid.Text = ds.Tables[0].Rows[0][3].ToString();            
            outcut.Text = ds.Tables[0].Rows[0][10].ToString();
            outlen.Focus();
            
        }
        catch { }
    }
    protected void OutLenchange2_Click(object sender, EventArgs e)
    {

        TextBox machine = (TextBox)FormView_Layout.FindControl("vMachineTextBox");
        Label machineDesc = (Label)FormView_Layout.FindControl("vMachDscrTextBox");
        TextBox frontback = (TextBox)FormView_Layout.FindControl("vFrontBackTextBox");
        TextBox sideside = (TextBox)FormView_Layout.FindControl("vSideSideTextBox");
        DropDownList revcorr = (DropDownList)FormView_Layout.FindControl("vRevCorrDropDown");
        TextBox board = (TextBox)FormView_Layout.FindControl("vBoardTextBox");
        TextBox boardname = (TextBox)FormView_Layout.FindControl("vBoardNameTextBox");
        TextBox costuom = (TextBox)FormView_Layout.FindControl("vCostUomTextBox");
        TextBox costmsf = (TextBox)FormView_Layout.FindControl("vCostMsfTextBox");
        TextBox freightuom = (TextBox)FormView_Layout.FindControl("vFreightUomTextBox");
        TextBox freightmsf = (TextBox)FormView_Layout.FindControl("vFreightCwtTextBox");
        //TextBox nc = (TextBox)FormView_Layout.FindControl("vNcTextBox");
        DropDownList dnc = (DropDownList)FormView_Layout.FindControl("ddl_nc");
        TextBox grosshetwid = (TextBox)FormView_Layout.FindControl("vGrosShetWidTextBox");
        TextBox grosshetlen = (TextBox)FormView_Layout.FindControl("vGrosShetLenTextBox");
        TextBox outwid = (TextBox)FormView_Layout.FindControl("vOutWidTextBox");
        TextBox outlen = (TextBox)FormView_Layout.FindControl("vOutLenTextBox");
        TextBox outcut = (TextBox)FormView_Layout.FindControl("vOutCutTextBox");
        TextBox dieinches = (TextBox)FormView_Layout.FindControl("vDieInchesTextBox");
        TextBox netshetwid = (TextBox)FormView_Layout.FindControl("vNetShetWidTextBox");
        TextBox netshetlen = (TextBox)FormView_Layout.FindControl("vNetShetLenTextBox");
        TextBox diesizewid = (TextBox)FormView_Layout.FindControl("vDieSizeWidTextBox");
        TextBox diesizelen = (TextBox)FormView_Layout.FindControl("vDieSizeLenTextBox");
        TextBox onwid = (TextBox)FormView_Layout.FindControl("vOnWidTextBox");
        TextBox onlen = (TextBox)FormView_Layout.FindControl("vOnLenTextBox");
        Label ontotalup = (Label)FormView_Layout.FindControl("vOnTotalUpLabel");
        Label style = (Label)FormView_Layout.FindControl("styleLabel");
        try
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Corrugated ord = new Corrugated();
            DataSet ds = new DataSet();

            ds = ord.CorrLayoutLen("OutLen", UserLogin.UserName, "", "", "", Convert.ToString(Session["order_corrugated_est"]), machine.Text.Trim(), Convert.ToInt32(Session["order_corrugated_formno"]), Convert.ToInt32(Session["order_corrugated_blankno"]), board.Text.Trim(), Convert.ToString(revcorr.SelectedValue), Convert.ToDecimal(outwid.Text.Trim()), Convert.ToDecimal(outlen.Text.Trim()), Convert.ToDecimal(onlen.Text.Trim()), Convert.ToDecimal(onwid.Text.Trim()), Convert.ToInt32(ontotalup.Text.Trim()), Convert.ToDecimal(grosshetlen.Text.Trim()), Convert.ToDecimal(grosshetwid.Text.Trim()), 0, Convert.ToDecimal(netshetlen.Text.Trim()), Convert.ToDecimal(netshetwid.Text.Trim()), 0);


            grosshetlen.Text = ds.Tables[0].Rows[0][2].ToString();
            grosshetwid.Text = ds.Tables[0].Rows[0][3].ToString();
            outcut.Text = ds.Tables[0].Rows[0][10].ToString();
            outcut.Focus();

        }
        catch { }
    }
    protected void Job_Button_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Session["corr_vendor_cost_est"] = Session["order_corrugated_est"];
        Session["corr_vendor_cost_form"] = Session["order_corrugated_formno"];
        Session["corr_vendor_cost_blank"] = Session["order_corrugated_blankno"];
        
        string vmessage = "";
        Corrugated corr = new Corrugated();
        corr.SelectPrep(UserLogin.UserName, "jobstd", "", "", Convert.ToString(Session["order_corrugated_est"]), Convert.ToInt32(Session["order_corrugated_formno"]), 0, 0, "", 0, "", 0, "", "", 0, 0, 0, 0, ref vmessage);

        if (vmessage != "")
        {

            Page.ClientScript.RegisterStartupScript(this.GetType(), "alert", "confirmAdd('" + vmessage + "');", true);

        }
        //ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "jobstd";
        //ObjectDataSource1.SelectParameters["prmLine"].DefaultValue = line.Text.Trim();

    }
}
