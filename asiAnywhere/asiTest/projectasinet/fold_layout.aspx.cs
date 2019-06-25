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

public partial class fold_layout : System.Web.UI.Page
{
    
    protected void Page_Load(object sender, EventArgs e)
    {
                
        FoldLayout_ObjectDataSource.SelectParameters["prmAction"].DefaultValue = "Select";
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "fold_layout.aspx";
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
            labelname.Text = "Folding";

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
            Label type = (Label)FormView_FoldLayout.FindControl("vTypeLabel");
            Session["fold_type_val"] = type.Text;
            
            Image img_mov_col = (Image)Master.FindControl("Image5");
            img_mov_col.Visible = false;

            Label roll = (Label)FormView_FoldLayout.FindControl("vRollLabel");
            //Response.Write(roll.Text);
            CheckBox chkroll = (CheckBox)FormView_FoldLayout.FindControl("chk_roll");
            if (roll.Text == "Y")
                chkroll.Checked = true;
            else
                chkroll.Checked = false;
        }
        catch { }
    }
    protected void FormView_FoldLayout_DataBound(object sender, EventArgs e)
    {
        if (FormView_FoldLayout.CurrentMode == FormViewMode.ReadOnly)
        {
            Button job = (Button)FormView_FoldLayout.FindControl("jobButton");
            UserClass UserLogin = (UserClass)Session["User"];
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "fold_layout.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;


                func1 f1 = new func1();
                //Response.Write(Page);
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                
                if (aUsers == "external")
                {
                    job.Visible = false;
                }
            }
        }
        if (FormView_FoldLayout.CurrentMode == FormViewMode.Edit)
        {
            TextBox roll = (TextBox)FormView_FoldLayout.FindControl("vRollTextBox");
            CheckBox chkroll = (CheckBox)FormView_FoldLayout.FindControl("chk_roll");
            if (roll.Text == "Y")
                chkroll.Checked = true;
            else
                chkroll.Checked = false;
           
            if (chkroll.Checked)
            {
                HiddenField1.Value = "Yes";

            }
            else
            {
                HiddenField1.Value = "N";

            }


            TextBox machine = (TextBox)FormView_FoldLayout.FindControl("vMachineTextBox");
            TextBox grosswid = (TextBox)FormView_FoldLayout.FindControl("vGrosShetWidTextBox");
            TextBox grosslen = (TextBox)FormView_FoldLayout.FindControl("vGrosShetLenTextBox");
            TextBox mfwid = (TextBox)FormView_FoldLayout.FindControl("vMachFeedWidTextBox");
            TextBox mflen = (TextBox)FormView_FoldLayout.FindControl("vMachFeedLenTextBox");
            TextBox diewid = (TextBox)FormView_FoldLayout.FindControl("vDieSizeWidTextBox");
            TextBox dielen = (TextBox)FormView_FoldLayout.FindControl("vDieSizeLenTextBox");
            TextBox frontback = (TextBox)FormView_FoldLayout.FindControl("vFrontBackTextBox");
            TextBox side = (TextBox)FormView_FoldLayout.FindControl("vSideSideTextBox");
            TextBox board = (TextBox)FormView_FoldLayout.FindControl("vBoardTextBox");
            TextBox costuom = (TextBox)FormView_FoldLayout.FindControl("vCostUomTextBox");
            TextBox costmsf = (TextBox)FormView_FoldLayout.FindControl("vCostMsfTextBox");
            TextBox die = (TextBox)FormView_FoldLayout.FindControl("vDieInchesTextBox");
            TextBox fuom = (TextBox)FormView_FoldLayout.FindControl("vFreightUomTextBox");
            TextBox fcwt = (TextBox)FormView_FoldLayout.FindControl("vFreightCwtTextBox");
            TextBox gwid = (TextBox)FormView_FoldLayout.FindControl("vGrosShetWidTextBox");
            TextBox glen = (TextBox)FormView_FoldLayout.FindControl("vGrosShetLenTextBox");
            TextBox outwid = (TextBox)FormView_FoldLayout.FindControl("vOutWidTextBox");
            TextBox outcut = (TextBox)FormView_FoldLayout.FindControl("vOutCutTextBox");
            TextBox mwid = (TextBox)FormView_FoldLayout.FindControl("vMachFeedWidTextBox");
            TextBox mlen = (TextBox)FormView_FoldLayout.FindControl("vMachFeedLenTextBox");
            TextBox outlen = (TextBox)FormView_FoldLayout.FindControl("vOutLenTextBox");
            TextBox dwid = (TextBox)FormView_FoldLayout.FindControl("vDieSizeWidTextBox");
            TextBox dlen = (TextBox)FormView_FoldLayout.FindControl("vDieSizeLenTextBox");
            TextBox onwid = (TextBox)FormView_FoldLayout.FindControl("vOnWidTextBox");
            TextBox onlen = (TextBox)FormView_FoldLayout.FindControl("vOnLenTextBox");

            DropDownList xgrain = (DropDownList)FormView_FoldLayout.FindControl("vXgrainDropDown");
            DropDownList nc = (DropDownList)FormView_FoldLayout.FindControl("ddl_nc");
            CheckBox rollw = (CheckBox)FormView_FoldLayout.FindControl("chk_roll");
            Image img_board = (Image)FormView_FoldLayout.FindControl("img_board");
            TextBox leaf1 = (TextBox)FormView_FoldLayout.FindControl("vLeaf1TextBox");
            TextBox leaf2 = (TextBox)FormView_FoldLayout.FindControl("vLeaf2TextBox");
            TextBox leaf3 = (TextBox)FormView_FoldLayout.FindControl("vLeaf3TextBox");
            TextBox leaf4 = (TextBox)FormView_FoldLayout.FindControl("vLeaf4TextBox");
            TextBox leafdesc1 = (TextBox)FormView_FoldLayout.FindControl("vLeafDesc1TextBox");
            TextBox leafdesc2 = (TextBox)FormView_FoldLayout.FindControl("vLeafDesc2TextBox");
            TextBox leafdesc3 = (TextBox)FormView_FoldLayout.FindControl("vLeafDesc3TextBox");
            TextBox leafdesc4 = (TextBox)FormView_FoldLayout.FindControl("vLeafDesc4TextBox");
            TextBox leafb1 = (TextBox)FormView_FoldLayout.FindControl("vLeafB1TextBox");
            TextBox leafb2 = (TextBox)FormView_FoldLayout.FindControl("vLeafB2TextBox");
            TextBox leafb3 = (TextBox)FormView_FoldLayout.FindControl("vLeafB3TextBox");
            TextBox leafb4 = (TextBox)FormView_FoldLayout.FindControl("vLeafB4TextBox");
            TextBox leafwid1 = (TextBox)FormView_FoldLayout.FindControl("vLeafWid1TextBox");
            TextBox leafwid2 = (TextBox)FormView_FoldLayout.FindControl("vLeafWid2TextBox");
            TextBox leafwid3 = (TextBox)FormView_FoldLayout.FindControl("vLeafWid3TextBox");
            TextBox leafwid4 = (TextBox)FormView_FoldLayout.FindControl("vLeafWid4TextBox");
            TextBox leaflen1 = (TextBox)FormView_FoldLayout.FindControl("vLeafLen1TextBox");
            TextBox leaflen2 = (TextBox)FormView_FoldLayout.FindControl("vLeafLen2TextBox");
            TextBox leaflen3 = (TextBox)FormView_FoldLayout.FindControl("vLeafLen3TextBox");
            TextBox leaflen4 = (TextBox)FormView_FoldLayout.FindControl("vLeafLen4TextBox");
            Label ontotalup = (Label)FormView_FoldLayout.FindControl("vOnTotalUpTextBox");
            Label machinedscr = (Label)FormView_FoldLayout.FindControl("vMachDscrTextBox");

            if (Session["fold_layout_auto_calc"] != null)
            {
                               
                grosslen.Enabled = false;
                grosswid.Enabled = false;
                mflen.Enabled = false;
                mfwid.Enabled = false;
                dielen.Enabled = false;
                diewid.Enabled = false;

                grosslen.BackColor = System.Drawing.Color.Turquoise;
                grosswid.BackColor = System.Drawing.Color.Turquoise;
                mflen.BackColor = System.Drawing.Color.Turquoise;
                mfwid.BackColor = System.Drawing.Color.Turquoise;
                dielen.BackColor = System.Drawing.Color.Turquoise;
                diewid.BackColor = System.Drawing.Color.Turquoise;
               

            }
            if (Session["sheet_calc"] != null)
            {
                Label sheetcal = (Label)FormView_FoldLayout.FindControl("sheetLabel");
                sheetcal.Text = "sheet";
                
                frontback.ReadOnly = true;
                side.ReadOnly = true;
                board.ReadOnly = true;
              
                costmsf.ReadOnly = true;
                costuom.ReadOnly = true;
                die.ReadOnly = true;
                fuom.ReadOnly = true;
                fcwt.ReadOnly = true;
                glen.ReadOnly = true;
                gwid.ReadOnly = true;
                outcut.ReadOnly=true;
                outlen.ReadOnly = true;
                outwid.ReadOnly = true;
                mwid.ReadOnly = true;
                mlen.ReadOnly = true;
                dwid.ReadOnly = true;
                dlen.ReadOnly = true;
                onwid.ReadOnly = true;
                onlen.ReadOnly = true;
                xgrain.Enabled = false;
                nc.Enabled = false;
                rollw.Enabled = false;
                img_board.Visible = false;

                frontback.BackColor = System.Drawing.Color.Turquoise;
                side.BackColor = System.Drawing.Color.Turquoise;
                board.BackColor = System.Drawing.Color.Turquoise;

                costmsf.BackColor = System.Drawing.Color.Turquoise;
                costuom.BackColor = System.Drawing.Color.Turquoise;
                die.BackColor = System.Drawing.Color.Turquoise;
                fuom.BackColor = System.Drawing.Color.Turquoise;
                fcwt.BackColor = System.Drawing.Color.Turquoise;
                glen.BackColor = System.Drawing.Color.Turquoise;
                gwid.BackColor = System.Drawing.Color.Turquoise;
                outcut.BackColor = System.Drawing.Color.Turquoise;
                outlen.BackColor = System.Drawing.Color.Turquoise;
                outwid.BackColor = System.Drawing.Color.Turquoise;
                mwid.BackColor = System.Drawing.Color.Turquoise;
                mlen.BackColor = System.Drawing.Color.Turquoise;
                dwid.BackColor = System.Drawing.Color.Turquoise;
                dlen.BackColor = System.Drawing.Color.Turquoise;
                onwid.BackColor = System.Drawing.Color.Turquoise;
                onlen.BackColor = System.Drawing.Color.Turquoise;
            }

            if (Convert.ToInt32(Session["fold_type_val"]) == 4 || Session["sheet_calc"] != null)
            {
               

                Image img1 = (Image)FormView_FoldLayout.FindControl("Image1");
                Image img2 = (Image)FormView_FoldLayout.FindControl("Image2");
                Image img3 = (Image)FormView_FoldLayout.FindControl("Image3");
                Image img4 = (Image)FormView_FoldLayout.FindControl("Image7");

                img1.Visible = false;
                img2.Visible = false;
                img3.Visible = false;
                img4.Visible = false;

                leaf1.ReadOnly = true;
                leaf2.ReadOnly = true;
                leaf3.ReadOnly = true;
                leaf4.ReadOnly = true;
                leafb1.ReadOnly = true;
                leafb2.ReadOnly = true;
                leafb3.ReadOnly = true;
                leafb4.ReadOnly = true;
                leafdesc1.ReadOnly = true;
                leafdesc2.ReadOnly = true;
                leafdesc3.ReadOnly = true;
                leafdesc4.ReadOnly = true;
                leaflen1.ReadOnly = true;
                leaflen2.ReadOnly = true;
                leaflen3.ReadOnly = true;
                leaflen4.ReadOnly = true;
                leafwid1.ReadOnly = true;
                leafwid2.ReadOnly = true;
                leafwid3.ReadOnly = true;
                leafwid4.ReadOnly = true;

                leaf1.BackColor = System.Drawing.Color.Turquoise;
                leaf2.BackColor = System.Drawing.Color.Turquoise;
                leaf3.BackColor = System.Drawing.Color.Turquoise;
                leaf4.BackColor = System.Drawing.Color.Turquoise;
                leafb1.BackColor = System.Drawing.Color.Turquoise;
                leafb2.BackColor = System.Drawing.Color.Turquoise;
                leafb3.BackColor = System.Drawing.Color.Turquoise;
                leafb4.BackColor = System.Drawing.Color.Turquoise;
                leafdesc1.BackColor = System.Drawing.Color.Turquoise;
                leafdesc2.BackColor = System.Drawing.Color.Turquoise;
                leafdesc3.BackColor = System.Drawing.Color.Turquoise;
                leafdesc4.BackColor = System.Drawing.Color.Turquoise;
                leaflen1.BackColor = System.Drawing.Color.Turquoise;
                leaflen2.BackColor = System.Drawing.Color.Turquoise;
                leaflen3.BackColor = System.Drawing.Color.Turquoise;
                leaflen4.BackColor = System.Drawing.Color.Turquoise;
                leafwid1.BackColor = System.Drawing.Color.Turquoise;
                leafwid2.BackColor = System.Drawing.Color.Turquoise;
                leafwid3.BackColor = System.Drawing.Color.Turquoise;
                leafwid4.BackColor = System.Drawing.Color.Turquoise;
            }
            if (frontback.Text == "")
                frontback.Text = "0";
            if (side.Text == "")
            {
                side.Text = "0";
            }
            try
            {
                if (Session["fold_layout_auto_calc"] != null)
                {
                    Corrugated cor = new Corrugated();
                    DataSet ds = new DataSet();
                    ds = cor.FoldLayMachine("FoldMachine", Convert.ToString(Session["User"]), Convert.ToString(Session["order_folding_est"]), Convert.ToInt32(Session["order_folding_formno"]), Convert.ToInt32(Session["order_folding_blankno"]), board.Text.Trim(), "", machine.Text.Trim(), xgrain.SelectedValue, HiddenField1.Value, Convert.ToDecimal(frontback.Text.Trim()), Convert.ToDecimal(side.Text.Trim()));

                    side.Text = ds.Tables[0].Rows[0][0].ToString();
                    frontback.Text = ds.Tables[0].Rows[0][1].ToString();
                    grosswid.Text = ds.Tables[0].Rows[0][3].ToString();
                    grosslen.Text = ds.Tables[0].Rows[0][2].ToString();
                    mflen.Text = ds.Tables[0].Rows[0][4].ToString();
                    mfwid.Text = ds.Tables[0].Rows[0][5].ToString();
                    dielen.Text = ds.Tables[0].Rows[0][6].ToString();
                    diewid.Text = ds.Tables[0].Rows[0][7].ToString();
                    onlen.Text = ds.Tables[0].Rows[0][8].ToString();
                    outcut.Text = ds.Tables[0].Rows[0][10].ToString();
                    die.Text = ds.Tables[0].Rows[0][15].ToString();
                    onwid.Text = ds.Tables[0].Rows[0][16].ToString();
                    onlen.Text = ds.Tables[0].Rows[0][17].ToString();
                    ontotalup.Text = ds.Tables[0].Rows[0][18].ToString();
                    machinedscr.Text = ds.Tables[0].Rows[0][21].ToString();
                }
                
            }
            catch { }
        }

    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        TextBox machine = (TextBox)FormView_FoldLayout.FindControl("vMachineTextBox");
        Label machinedscr = (Label)FormView_FoldLayout.FindControl("vMachDscrTextBox");
        TextBox frontback = (TextBox)FormView_FoldLayout.FindControl("vFrontBackTextBox");
        TextBox sideside = (TextBox)FormView_FoldLayout.FindControl("vSideSideTextBox");
        DropDownList xgrain = (DropDownList)FormView_FoldLayout.FindControl("vXgrainDropDown");
        TextBox board = (TextBox)FormView_FoldLayout.FindControl("vBoardTextBox");
        Label boarddesc = (Label)FormView_FoldLayout.FindControl("vBoardNameTextBox");
        Label real = (Label)FormView_FoldLayout.FindControl("vRealTextBox");
        Label caliper = (Label)FormView_FoldLayout.FindControl("vCaliperTextBox");
        TextBox costmsf = (TextBox)FormView_FoldLayout.FindControl("vCostMsfTextBox");
        TextBox costuom = (TextBox)FormView_FoldLayout.FindControl("vCostUomTextBox");
        Label weight = (Label)FormView_FoldLayout.FindControl("vWeightTextBox");
        TextBox freightcwt = (TextBox)FormView_FoldLayout.FindControl("vFreightCwtTextBox");
        TextBox freightuom = (TextBox)FormView_FoldLayout.FindControl("vFreightUomTextBox");
        DropDownList nc = (DropDownList)FormView_FoldLayout.FindControl("ddl_nc");
        Label rollwid = (Label)FormView_FoldLayout.FindControl("vRollWidTextBox");
        TextBox gswid = (TextBox)FormView_FoldLayout.FindControl("vGrosShetWidTextBox");
        TextBox gslen = (TextBox)FormView_FoldLayout.FindControl("vGrosShetLenTextBox");
        TextBox outwid = (TextBox)FormView_FoldLayout.FindControl("vOutWidTextBox");
        TextBox outcut = (TextBox)FormView_FoldLayout.FindControl("vOutCutTextBox");
        TextBox mfwid = (TextBox)FormView_FoldLayout.FindControl("vMachFeedWidTextBox");
        TextBox mflen = (TextBox)FormView_FoldLayout.FindControl("vMachFeedLenTextBox");
        TextBox outlen = (TextBox)FormView_FoldLayout.FindControl("vOutLenTextBox");
        TextBox diewid = (TextBox)FormView_FoldLayout.FindControl("vDieSizeWidTextBox");
        TextBox dielen = (TextBox)FormView_FoldLayout.FindControl("vDieSizeLenTextBox");
        TextBox onwid = (TextBox)FormView_FoldLayout.FindControl("vOnWidTextBox");
        TextBox onlen = (TextBox)FormView_FoldLayout.FindControl("vOnLenTextBox");
        Label ontotalup = (Label)FormView_FoldLayout.FindControl("vOnTotalUpTextBox");
        TextBox dieinches = (TextBox)FormView_FoldLayout.FindControl("vDieInchesTextBox");
        Label blankwid = (Label)FormView_FoldLayout.FindControl("vBlankWidTextBox");
        Label blanklen = (Label)FormView_FoldLayout.FindControl("vBlankLenTextBox");
        Label blanksqin = (Label)FormView_FoldLayout.FindControl("vBlankSqInchTextBox");
        TextBox leaf1 = (TextBox)FormView_FoldLayout.FindControl("vLeaf1TextBox");
        TextBox leafdesc1 = (TextBox)FormView_FoldLayout.FindControl("vLeafDesc1TextBox");
        //TextBox leafs1 = (TextBox)FormView_FoldLayout.FindControl("vLeafS1TextBox");
        TextBox leafb1 = (TextBox)FormView_FoldLayout.FindControl("vLeafB1TextBox");
        TextBox leafwid1 = (TextBox)FormView_FoldLayout.FindControl("vLeafWid1TextBox");
        TextBox leaflen1 = (TextBox)FormView_FoldLayout.FindControl("vLeafLen1TextBox");

        TextBox leaf2 = (TextBox)FormView_FoldLayout.FindControl("vLeaf2TextBox");
        TextBox leafdesc2 = (TextBox)FormView_FoldLayout.FindControl("vLeafDesc2TextBox");
        //TextBox leafs2 = (TextBox)FormView_FoldLayout.FindControl("vLeafS2TextBox");
        TextBox leafb2 = (TextBox)FormView_FoldLayout.FindControl("vLeafB2TextBox");
        TextBox leafwid2 = (TextBox)FormView_FoldLayout.FindControl("vLeafWid2TextBox");
        TextBox leaflen2 = (TextBox)FormView_FoldLayout.FindControl("vLeafLen2TextBox");

        TextBox leaf3 = (TextBox)FormView_FoldLayout.FindControl("vLeaf3TextBox");
        TextBox leafdesc3 = (TextBox)FormView_FoldLayout.FindControl("vLeafDesc3TextBox");
        //TextBox leafs3 = (TextBox)FormView_FoldLayout.FindControl("vLeafS3TextBox");
        TextBox leafb3 = (TextBox)FormView_FoldLayout.FindControl("vLeafB3TextBox");
        TextBox leafwid3 = (TextBox)FormView_FoldLayout.FindControl("vLeafWid3TextBox");
        TextBox leaflen3 = (TextBox)FormView_FoldLayout.FindControl("vLeafLen3TextBox");

        TextBox leaf4 = (TextBox)FormView_FoldLayout.FindControl("vLeaf4TextBox");
        TextBox leafdesc4 = (TextBox)FormView_FoldLayout.FindControl("vLeafDesc4TextBox");
       // TextBox leafs4 = (TextBox)FormView_FoldLayout.FindControl("vLeafS4TextBox");
        TextBox leafb4 = (TextBox)FormView_FoldLayout.FindControl("vLeafB4TextBox");
        TextBox leafwid4 = (TextBox)FormView_FoldLayout.FindControl("vLeafWid4TextBox");
        TextBox leaflen4 = (TextBox)FormView_FoldLayout.FindControl("vLeafLen4TextBox");

        CheckBox roll = (CheckBox)FormView_FoldLayout.FindControl("chk_roll");
        if (roll.Checked)
        {
            HiddenField1.Value = "Y";
            rollwid.Text = gswid.Text;
        }
        else
        {
            HiddenField1.Value = "N";
            rollwid.Text = "0.0000";
        }
        if (leafwid1.Text == "")
            leafwid1.Text = "0";
        if (leafwid2.Text == "")
            leafwid2.Text = "0";
        if (leafwid3.Text == "")
            leafwid3.Text = "0";
        if (leafwid4.Text == "")
            leafwid4.Text = "0";
        if (leaflen1.Text == "")
            leaflen1.Text = "0";
        if (leaflen2.Text == "")
            leaflen2.Text = "0";
        if (leaflen3.Text == "")
            leaflen3.Text = "0";
        if (leaflen4.Text == "")
            leaflen4.Text = "0";
        if (ontotalup.Text == "")
            ontotalup.Text = "0";
        if (frontback.Text == "")
            frontback.Text = "0";
        if (sideside.Text == "")
            sideside.Text = "0";
        if (costmsf.Text == "")
            costmsf.Text = "0";
        if (rollwid.Text == "")
            rollwid.Text = "0";
        if (gswid.Text == "")
            gswid.Text = "0";
        if (gslen.Text == "")
            gslen.Text = "0";
        if (outwid.Text == "")
            outwid.Text = "0";
        if (outlen.Text == "")
            outlen.Text = "0";
        if (outcut.Text == "")
            outcut.Text = "0";
        if (dieinches.Text == "")
            dieinches.Text = "0";
        if (mfwid.Text == "")
            mfwid.Text = "0";
        if (mflen.Text == "")
            mflen.Text = "0";
        if (diewid.Text == "")
            diewid.Text = "0";
        if (dielen.Text == "")
            dielen.Text = "0";
        if (onwid.Text == "")
            onwid.Text = "0";

        if (onlen.Text == "")
            onlen.Text = "0";
        if (freightcwt.Text == "")
            freightcwt.Text = "0";

        try
        {
            Corrugated corr = new Corrugated();

            bool check = corr.ValidateFoldLayout(Convert.ToString(Session["User"]), "ValidateOverRide", "", "", Convert.ToString(Session["order_folding_est"]), Convert.ToDateTime("12/11/2009"), Convert.ToInt32(Session["order_folding_formno"]), 0, "", machine.Text.Trim(), "", Convert.ToDecimal(frontback.Text.Trim()), Convert.ToDecimal(sideside.Text.Trim()), xgrain.SelectedValue, board.Text, "", "", 0, Convert.ToDecimal(costmsf.Text.Trim()), costuom.Text, 0, Convert.ToDecimal(freightcwt.Text), freightuom.Text, nc.Text, Convert.ToString(HiddenField1.Value), Convert.ToDecimal(rollwid.Text), Convert.ToDecimal(gswid.Text), Convert.ToDecimal(gslen.Text), Convert.ToDecimal(outwid.Text), Convert.ToDecimal(outlen.Text), Convert.ToInt32(outcut.Text), Convert.ToDecimal(dieinches.Text), Convert.ToDecimal(mfwid.Text), Convert.ToDecimal(mflen.Text), Convert.ToDecimal(diewid.Text), Convert.ToDecimal(dielen.Text), Convert.ToDecimal(onwid.Text), Convert.ToDecimal(onlen.Text), Convert.ToInt32(ontotalup.Text), 0, 0, 0, leaf1.Text, leaf2.Text, leaf3.Text, leaf4.Text, "", "", "", "", 0, 0, Convert.ToDecimal(leafwid1.Text), Convert.ToDecimal(leaflen1.Text), 0, 0, Convert.ToDecimal(leafwid2.Text), Convert.ToDecimal(leaflen2.Text), 0, 0, Convert.ToDecimal(leafwid3.Text), Convert.ToDecimal(leaflen3.Text), 0, 0, Convert.ToDecimal(leafwid4.Text), Convert.ToDecimal(leaflen4.Text), Convert.ToInt32(Session["order_folding_blankno"]), "");

        
        string value = Convert.ToString(check);
       
        if (value == "True")
        {

            FoldLayout_ObjectDataSource.SelectParameters["prmAction"].DefaultValue = "OverRide";

            FoldLayout_ObjectDataSource.SelectParameters["prmMachine"].DefaultValue = machine.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmMachDscr"].DefaultValue = machinedscr.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmFrontBack"].DefaultValue = frontback.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmSideSide"].DefaultValue = sideside.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmXgrain"].DefaultValue = xgrain.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmBoard"].DefaultValue = board.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmBoardName"].DefaultValue = boarddesc.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmReal"].DefaultValue = real.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmCaliper"].DefaultValue = caliper.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmCostMsf"].DefaultValue = costmsf.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmCostUom"].DefaultValue = costuom.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmWeightt"].DefaultValue = weight.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmFreightMsf"].DefaultValue = freightcwt.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmFreightUom"].DefaultValue = freightuom.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmNc"].DefaultValue = nc.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmRoll"].DefaultValue = HiddenField1.Value;
            FoldLayout_ObjectDataSource.SelectParameters["prmRollWid"].DefaultValue = rollwid.Text;
            FoldLayout_ObjectDataSource.SelectParameters["prmGrosShetWid"].DefaultValue = gswid.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmGrosShetLen"].DefaultValue = gslen.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmOutWid"].DefaultValue = outwid.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmOutLen"].DefaultValue = outlen.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmOutCut"].DefaultValue = outcut.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmDieInches"].DefaultValue = dieinches.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmMachFeedWid"].DefaultValue = mfwid.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmMachFeedLen"].DefaultValue = mflen.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmDieSizeWid"].DefaultValue = diewid.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmDieSizeLen"].DefaultValue = dielen.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmOnWid"].DefaultValue = onwid.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmOnLen"].DefaultValue = onlen.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmOnTotalUp"].DefaultValue = ontotalup.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmBlankWid"].DefaultValue = blankwid.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmBlankLen"].DefaultValue = blanklen.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmBlankSqInch"].DefaultValue = blanksqin.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmLeaf1"].DefaultValue = leaf1.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmLeaf2"].DefaultValue = leaf2.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmLeaf3"].DefaultValue = leaf3.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmLeaf4"].DefaultValue = leaf4.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmLeafDesc1"].DefaultValue = leafdesc1.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmLeafDesc2"].DefaultValue = leafdesc2.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmLeafDesc3"].DefaultValue = leafdesc3.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmLeafDesc4"].DefaultValue = leafdesc4.Text.Trim();
            //FoldLayout_ObjectDataSource.SelectParameters["prmS1"].DefaultValue = leafs1.Text.Trim();
            //FoldLayout_ObjectDataSource.SelectParameters["prmS2"].DefaultValue = leafs2.Text.Trim();
            //FoldLayout_ObjectDataSource.SelectParameters["prmS3"].DefaultValue = leafs3.Text.Trim();
            //FoldLayout_ObjectDataSource.SelectParameters["prmS4"].DefaultValue = leafs4.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmB1"].DefaultValue = leafb1.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmB2"].DefaultValue = leafb2.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmB3"].DefaultValue = leafb3.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmB4"].DefaultValue = leafb4.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmLeafWid1"].DefaultValue = leafwid1.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmLeafWid2"].DefaultValue = leafwid2.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmLeafWid3"].DefaultValue = leafwid3.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmLeafWid4"].DefaultValue = leafwid4.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmLeafLen1"].DefaultValue = leaflen1.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmLeafLen2"].DefaultValue = leaflen2.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmLeafLen3"].DefaultValue = leaflen3.Text.Trim();
            FoldLayout_ObjectDataSource.SelectParameters["prmLeafLen4"].DefaultValue = leaflen4.Text.Trim();
            if (Session["fold_layout_auto_calc"] != "")
            {
                FoldLayout_ObjectDataSource.SelectParameters["prmAuto"].DefaultValue = "Yes";
            }

            Session["fold_type_val"] = null;
            Session["sheet_calc"] = null;
            Session["fold_layout_auto_calc"] = null;
            FormView_FoldLayout.ChangeMode(FormViewMode.ReadOnly);
        }
        }
        catch { }
    }

    protected void btn_sheet_calc_click(object sender, EventArgs e)
    {
        Session["sheet_calc"] = 1;
        Session["fold_layout_auto_calc"] = null;
       
    }
    protected void btn_update_cancel(object sender, EventArgs e)
    {
        Session["sheet_calc"] = null;
        Session["fold_layout_auto_calc"] = null;
      
    }
    protected void btn_auto_calc_click(object sender, EventArgs e)
    {
        Session["fold_layout_auto_calc"] = 1;
        Session["sheet_calc"] = null;
       
    }
    protected void OverRide_button_click(object sender, EventArgs e)
    {
        
        
    }

    protected void machine_text_change(object sender, EventArgs e)
    {
        TextBox machine = (TextBox)FormView_FoldLayout.FindControl("vMachineTextBox");
        Label machinedscr = (Label)FormView_FoldLayout.FindControl("vMachDscrTextBox");
        TextBox frontback = (TextBox)FormView_FoldLayout.FindControl("vFrontBackTextBox");
        TextBox sideside = (TextBox)FormView_FoldLayout.FindControl("vSideSideTextBox");
        DropDownList xgrain = (DropDownList)FormView_FoldLayout.FindControl("vXgrainDropDown");
        TextBox board = (TextBox)FormView_FoldLayout.FindControl("vBoardTextBox");
        Label boarddesc = (Label)FormView_FoldLayout.FindControl("vBoardNameTextBox");
        Label real = (Label)FormView_FoldLayout.FindControl("vRealTextBox");
        Label caliper = (Label)FormView_FoldLayout.FindControl("vCaliperTextBox");
        TextBox costmsf = (TextBox)FormView_FoldLayout.FindControl("vCostMsfTextBox");
        TextBox costuom = (TextBox)FormView_FoldLayout.FindControl("vCostUomTextBox");
        Label weight = (Label)FormView_FoldLayout.FindControl("vWeightTextBox");
        TextBox freightcwt = (TextBox)FormView_FoldLayout.FindControl("vFreightCwtTextBox");
        TextBox freightuom = (TextBox)FormView_FoldLayout.FindControl("vFreightUomTextBox");
        DropDownList nc = (DropDownList)FormView_FoldLayout.FindControl("ddl_nc");
        Label rollwid = (Label)FormView_FoldLayout.FindControl("vRollWidTextBox");
        TextBox gswid = (TextBox)FormView_FoldLayout.FindControl("vGrosShetWidTextBox");
        TextBox gslen = (TextBox)FormView_FoldLayout.FindControl("vGrosShetLenTextBox");
        TextBox outwid = (TextBox)FormView_FoldLayout.FindControl("vOutWidTextBox");
        TextBox outcut = (TextBox)FormView_FoldLayout.FindControl("vOutCutTextBox");
        TextBox mfwid = (TextBox)FormView_FoldLayout.FindControl("vMachFeedWidTextBox");
        TextBox mflen = (TextBox)FormView_FoldLayout.FindControl("vMachFeedLenTextBox");
        TextBox outlen = (TextBox)FormView_FoldLayout.FindControl("vOutLenTextBox");
        TextBox diewid = (TextBox)FormView_FoldLayout.FindControl("vDieSizeWidTextBox");
        TextBox dielen = (TextBox)FormView_FoldLayout.FindControl("vDieSizeLenTextBox");
        TextBox onwid = (TextBox)FormView_FoldLayout.FindControl("vOnWidTextBox");
        TextBox onlen = (TextBox)FormView_FoldLayout.FindControl("vOnLenTextBox");
        Label ontotalup = (Label)FormView_FoldLayout.FindControl("vOnTotalUpTextBox");
        TextBox dieinches = (TextBox)FormView_FoldLayout.FindControl("vDieInchesTextBox");
        Label blankwid = (Label)FormView_FoldLayout.FindControl("vBlankWidTextBox");
        Label blanklen = (Label)FormView_FoldLayout.FindControl("vBlankLenTextBox");
        Label blanksqin = (Label)FormView_FoldLayout.FindControl("vBlankSqInchTextBox");
        
         CheckBox roll = (CheckBox)FormView_FoldLayout.FindControl("chk_roll");
        if (roll.Checked)
        {
            HiddenField1.Value = "Yes";
            
        }
        else
        {
            HiddenField1.Value = "N";
          
        }

        if (frontback.Text == "")
            frontback.Text = "0";
        if (sideside.Text == "")
        {
            sideside.Text = "0";
        }
        try
        {
            if (Session["fold_layout_auto_calc"] != null)
            {
                Corrugated cor = new Corrugated();
                DataSet ds = new DataSet();
                ds = cor.FoldLayMachine("FoldMachine", Convert.ToString(Session["User"]), Convert.ToString(Session["order_folding_est"]), Convert.ToInt32(Session["order_folding_formno"]), Convert.ToInt32(Session["order_folding_blankno"]), board.Text.Trim(), "", machine.Text.Trim(), xgrain.SelectedValue, HiddenField1.Value, Convert.ToDecimal(frontback.Text.Trim()), Convert.ToDecimal(sideside.Text.Trim()));

                sideside.Text = ds.Tables[0].Rows[0][0].ToString();
                frontback.Text = ds.Tables[0].Rows[0][1].ToString();
                gslen.Text = ds.Tables[0].Rows[0][2].ToString();
                gswid.Text = ds.Tables[0].Rows[0][3].ToString();
                mflen.Text = ds.Tables[0].Rows[0][4].ToString();
                mfwid.Text = ds.Tables[0].Rows[0][5].ToString();
                dielen.Text = ds.Tables[0].Rows[0][6].ToString();
                diewid.Text = ds.Tables[0].Rows[0][7].ToString();
                onlen.Text = ds.Tables[0].Rows[0][8].ToString();
                outcut.Text = ds.Tables[0].Rows[0][10].ToString();
                dieinches.Text = ds.Tables[0].Rows[0][15].ToString();
                onwid.Text = ds.Tables[0].Rows[0][16].ToString();
                onlen.Text = ds.Tables[0].Rows[0][17].ToString();
                ontotalup.Text = ds.Tables[0].Rows[0][18].ToString();
                machinedscr.Text = ds.Tables[0].Rows[0][21].ToString();
            }
        }
        catch { }
    }

    protected void board_text_change(object sender, EventArgs e)
    {

        TextBox machine = (TextBox)FormView_FoldLayout.FindControl("vMachineTextBox");
        Label machinedscr = (Label)FormView_FoldLayout.FindControl("vMachDscrTextBox");
        TextBox frontback = (TextBox)FormView_FoldLayout.FindControl("vFrontBackTextBox");
        TextBox sideside = (TextBox)FormView_FoldLayout.FindControl("vSideSideTextBox");
        DropDownList xgrain = (DropDownList)FormView_FoldLayout.FindControl("vXgrainDropDown");
        TextBox board = (TextBox)FormView_FoldLayout.FindControl("vBoardTextBox");
        Label boarddesc = (Label)FormView_FoldLayout.FindControl("vBoardNameTextBox");
        Label real = (Label)FormView_FoldLayout.FindControl("vRealTextBox");
        Label caliper = (Label)FormView_FoldLayout.FindControl("vCaliperTextBox");
        TextBox costmsf = (TextBox)FormView_FoldLayout.FindControl("vCostMsfTextBox");
        TextBox costuom = (TextBox)FormView_FoldLayout.FindControl("vCostUomTextBox");
        Label weight = (Label)FormView_FoldLayout.FindControl("vWeightTextBox");
        TextBox freightcwt = (TextBox)FormView_FoldLayout.FindControl("vFreightCwtTextBox");
        TextBox freightuom = (TextBox)FormView_FoldLayout.FindControl("vFreightUomTextBox");
        DropDownList nc = (DropDownList)FormView_FoldLayout.FindControl("ddl_nc");
        Label rollwid = (Label)FormView_FoldLayout.FindControl("vRollWidTextBox");
        TextBox gswid = (TextBox)FormView_FoldLayout.FindControl("vGrosShetWidTextBox");
        TextBox gslen = (TextBox)FormView_FoldLayout.FindControl("vGrosShetLenTextBox");
        TextBox outwid = (TextBox)FormView_FoldLayout.FindControl("vOutWidTextBox");
        TextBox outcut = (TextBox)FormView_FoldLayout.FindControl("vOutCutTextBox");
        TextBox mfwid = (TextBox)FormView_FoldLayout.FindControl("vMachFeedWidTextBox");
        TextBox mflen = (TextBox)FormView_FoldLayout.FindControl("vMachFeedLenTextBox");
        TextBox outlen = (TextBox)FormView_FoldLayout.FindControl("vOutLenTextBox");
        TextBox diewid = (TextBox)FormView_FoldLayout.FindControl("vDieSizeWidTextBox");
        TextBox dielen = (TextBox)FormView_FoldLayout.FindControl("vDieSizeLenTextBox");
        TextBox onwid = (TextBox)FormView_FoldLayout.FindControl("vOnWidTextBox");
        TextBox onlen = (TextBox)FormView_FoldLayout.FindControl("vOnLenTextBox");
        Label ontotalup = (Label)FormView_FoldLayout.FindControl("vOnTotalUpTextBox");
        TextBox dieinches = (TextBox)FormView_FoldLayout.FindControl("vDieInchesTextBox");
        Label blankwid = (Label)FormView_FoldLayout.FindControl("vBlankWidTextBox");
        Label blanklen = (Label)FormView_FoldLayout.FindControl("vBlankLenTextBox");
        Label blanksqin = (Label)FormView_FoldLayout.FindControl("vBlankSqInchTextBox");
        TextBox leaf1 = (TextBox)FormView_FoldLayout.FindControl("vLeaf1TextBox");
        TextBox leafdesc1 = (TextBox)FormView_FoldLayout.FindControl("vLeafDesc1TextBox");
        
        CheckBox roll = (CheckBox)FormView_FoldLayout.FindControl("chk_roll");
        if (roll.Checked)
        {
            HiddenField1.Value = "Yes";

        }
        else
        {
            HiddenField1.Value = "N";

        }

        if (frontback.Text == "")
            frontback.Text = "0";
        if (sideside.Text == "")
        {
            sideside.Text = "0";
        }
        if (Session["fold_layout_auto_calc"] != null)
        {
            try
            {
                Corrugated cor = new Corrugated();
                DataSet ds = new DataSet();
                ds = cor.FoldLayMachine("FoldBoard", Convert.ToString(Session["User"]), Convert.ToString(Session["order_folding_est"]), Convert.ToInt32(Session["order_folding_formno"]), Convert.ToInt32(Session["order_folding_blankno"]), board.Text.Trim(), "", machine.Text.Trim(), xgrain.SelectedValue, HiddenField1.Value, Convert.ToDecimal(frontback.Text.Trim()), Convert.ToDecimal(sideside.Text.Trim()));

                sideside.Text = ds.Tables[0].Rows[0][0].ToString();
                frontback.Text = ds.Tables[0].Rows[0][1].ToString();
                gslen.Text = ds.Tables[0].Rows[0][2].ToString();
                gswid.Text = ds.Tables[0].Rows[0][3].ToString();
                mflen.Text = ds.Tables[0].Rows[0][4].ToString();
                mfwid.Text = ds.Tables[0].Rows[0][5].ToString();
                dielen.Text = ds.Tables[0].Rows[0][6].ToString();
                diewid.Text = ds.Tables[0].Rows[0][7].ToString();
                onlen.Text = ds.Tables[0].Rows[0][8].ToString();
                outcut.Text = ds.Tables[0].Rows[0][10].ToString();
                //dieinches.Text = ds.Tables[0].Rows[0][15].ToString();
                onwid.Text = ds.Tables[0].Rows[0][16].ToString();
                onlen.Text = ds.Tables[0].Rows[0][17].ToString();
                ontotalup.Text = ds.Tables[0].Rows[0][18].ToString();
                rollwid.Text = ds.Tables[0].Rows[0][20].ToString();
                if (ds.Tables[0].Rows[0][15].ToString() == "Y")
                    roll.Checked = true;
                if (ds.Tables[0].Rows[0][15].ToString() == "N")
                    roll.Checked = false;
                caliper.Text = ds.Tables[0].Rows[0][22].ToString();
            }
            catch { }
        } 
    }

    protected void onwid_text_change(object sender, EventArgs e)
    {
        TextBox machine = (TextBox)FormView_FoldLayout.FindControl("vMachineTextBox");
        Label machinedscr = (Label)FormView_FoldLayout.FindControl("vMachDscrTextBox");
        TextBox frontback = (TextBox)FormView_FoldLayout.FindControl("vFrontBackTextBox");
        TextBox sideside = (TextBox)FormView_FoldLayout.FindControl("vSideSideTextBox");
        DropDownList xgrain = (DropDownList)FormView_FoldLayout.FindControl("vXgrainDropDown");
        TextBox board = (TextBox)FormView_FoldLayout.FindControl("vBoardTextBox");
        Label boarddesc = (Label)FormView_FoldLayout.FindControl("vBoardNameTextBox");
        Label real = (Label)FormView_FoldLayout.FindControl("vRealTextBox");
        Label caliper = (Label)FormView_FoldLayout.FindControl("vCaliperTextBox");
        TextBox costmsf = (TextBox)FormView_FoldLayout.FindControl("vCostMsfTextBox");
        TextBox costuom = (TextBox)FormView_FoldLayout.FindControl("vCostUomTextBox");
        Label weight = (Label)FormView_FoldLayout.FindControl("vWeightTextBox");
        TextBox freightcwt = (TextBox)FormView_FoldLayout.FindControl("vFreightCwtTextBox");
        TextBox freightuom = (TextBox)FormView_FoldLayout.FindControl("vFreightUomTextBox");
        DropDownList nc = (DropDownList)FormView_FoldLayout.FindControl("ddl_nc");
        Label rollwid = (Label)FormView_FoldLayout.FindControl("vRollWidTextBox");
        TextBox gswid = (TextBox)FormView_FoldLayout.FindControl("vGrosShetWidTextBox");
        TextBox gslen = (TextBox)FormView_FoldLayout.FindControl("vGrosShetLenTextBox");
        TextBox outwid = (TextBox)FormView_FoldLayout.FindControl("vOutWidTextBox");
        TextBox outcut = (TextBox)FormView_FoldLayout.FindControl("vOutCutTextBox");
        TextBox mfwid = (TextBox)FormView_FoldLayout.FindControl("vMachFeedWidTextBox");
        TextBox mflen = (TextBox)FormView_FoldLayout.FindControl("vMachFeedLenTextBox");
        TextBox outlen = (TextBox)FormView_FoldLayout.FindControl("vOutLenTextBox");
        TextBox diewid = (TextBox)FormView_FoldLayout.FindControl("vDieSizeWidTextBox");
        TextBox dielen = (TextBox)FormView_FoldLayout.FindControl("vDieSizeLenTextBox");
        TextBox onwid = (TextBox)FormView_FoldLayout.FindControl("vOnWidTextBox");
        TextBox onlen = (TextBox)FormView_FoldLayout.FindControl("vOnLenTextBox");
        Label ontotalup = (Label)FormView_FoldLayout.FindControl("vOnTotalUpTextBox");
        TextBox dieinches = (TextBox)FormView_FoldLayout.FindControl("vDieInchesTextBox");
        Label blankwid = (Label)FormView_FoldLayout.FindControl("vBlankWidTextBox");
        Label blanklen = (Label)FormView_FoldLayout.FindControl("vBlankLenTextBox");
        Label blanksqin = (Label)FormView_FoldLayout.FindControl("vBlankSqInchTextBox");
        TextBox leaf1 = (TextBox)FormView_FoldLayout.FindControl("vLeaf1TextBox");
        TextBox leafdesc1 = (TextBox)FormView_FoldLayout.FindControl("vLeafDesc1TextBox");
        CheckBox roll = (CheckBox)FormView_FoldLayout.FindControl("chk_roll");
        Label type = (Label)FormView_FoldLayout.FindControl("typelabel");
        if (roll.Checked)
        {
            HiddenField1.Value = "Yes";

        }
        else
        {
            HiddenField1.Value = "N";

        }

        if (frontback.Text == "")
            frontback.Text = "0";
        if (sideside.Text == "")
        {
            sideside.Text = "0";
        }


        if (Session["fold_layout_auto_calc"] != null)
        {
            try
            {
                Corrugated cor = new Corrugated();
                DataSet ds = new DataSet();
                ds = cor.CorrLayoutLen("Fold-On-Len", Convert.ToString(Session["User"]), "", "", "", Convert.ToString(Session["order_folding_est"]), machine.Text, Convert.ToInt32(Session["order_folding_formno"]), Convert.ToInt32(Session["order_folding_blankno"]), board.Text.Trim(), xgrain.SelectedValue, Convert.ToDecimal(outwid.Text), Convert.ToDecimal(outlen.Text), Convert.ToDecimal(onlen.Text), Convert.ToDecimal(onwid.Text), 0, Convert.ToDecimal(gslen.Text), Convert.ToDecimal(gswid.Text), 0, Convert.ToDecimal(mflen.Text), Convert.ToDecimal(mfwid.Text), 0);
                sideside.Text = ds.Tables[0].Rows[0][0].ToString();
                frontback.Text = ds.Tables[0].Rows[0][1].ToString();
                gslen.Text = ds.Tables[0].Rows[0][2].ToString();
                gswid.Text = ds.Tables[0].Rows[0][3].ToString();
                mflen.Text = ds.Tables[0].Rows[0][4].ToString();
                mfwid.Text = ds.Tables[0].Rows[0][5].ToString();
                dielen.Text = ds.Tables[0].Rows[0][6].ToString();
                diewid.Text = ds.Tables[0].Rows[0][7].ToString();
                outwid.Text = ds.Tables[0].Rows[0][8].ToString();
                outcut.Text = ds.Tables[0].Rows[0][10].ToString();
                dieinches.Text = ds.Tables[0].Rows[0][19].ToString();
                onwid.Text = ds.Tables[0].Rows[0][15].ToString();
                onlen.Text = ds.Tables[0].Rows[0][16].ToString();
                ontotalup.Text = ds.Tables[0].Rows[0][18].ToString();

               // Response.Write(ds.Tables[0].Rows[0][19].ToString());
               
            }
            catch { }
        }
        else
        {
            
            if (Convert.ToDecimal(onwid.Text) < 0)
                onwid.Text = "1";
            if (Convert.ToDecimal(onlen.Text) < 0)
                onlen.Text = "1";

            ontotalup.Text = Convert.ToString(Convert.ToDecimal(onwid.Text) * Convert.ToDecimal(onlen.Text));
                      
        }

    }
    protected void outwid_text_change(object sender, EventArgs e)
    {
        TextBox machine = (TextBox)FormView_FoldLayout.FindControl("vMachineTextBox");
        Label machinedscr = (Label)FormView_FoldLayout.FindControl("vMachDscrTextBox");
        TextBox frontback = (TextBox)FormView_FoldLayout.FindControl("vFrontBackTextBox");
        TextBox sideside = (TextBox)FormView_FoldLayout.FindControl("vSideSideTextBox");
        DropDownList xgrain = (DropDownList)FormView_FoldLayout.FindControl("vXgrainDropDown");
        TextBox board = (TextBox)FormView_FoldLayout.FindControl("vBoardTextBox");
        Label boarddesc = (Label)FormView_FoldLayout.FindControl("vBoardNameTextBox");
        Label real = (Label)FormView_FoldLayout.FindControl("vRealTextBox");
        Label caliper = (Label)FormView_FoldLayout.FindControl("vCaliperTextBox");
        TextBox costmsf = (TextBox)FormView_FoldLayout.FindControl("vCostMsfTextBox");
        TextBox costuom = (TextBox)FormView_FoldLayout.FindControl("vCostUomTextBox");
        Label weight = (Label)FormView_FoldLayout.FindControl("vWeightTextBox");
        TextBox freightcwt = (TextBox)FormView_FoldLayout.FindControl("vFreightCwtTextBox");
        TextBox freightuom = (TextBox)FormView_FoldLayout.FindControl("vFreightUomTextBox");
        DropDownList nc = (DropDownList)FormView_FoldLayout.FindControl("ddl_nc");
        Label rollwid = (Label)FormView_FoldLayout.FindControl("vRollWidTextBox");
        TextBox gswid = (TextBox)FormView_FoldLayout.FindControl("vGrosShetWidTextBox");
        TextBox gslen = (TextBox)FormView_FoldLayout.FindControl("vGrosShetLenTextBox");
        TextBox outwid = (TextBox)FormView_FoldLayout.FindControl("vOutWidTextBox");
        TextBox outcut = (TextBox)FormView_FoldLayout.FindControl("vOutCutTextBox");
        TextBox mfwid = (TextBox)FormView_FoldLayout.FindControl("vMachFeedWidTextBox");
        TextBox mflen = (TextBox)FormView_FoldLayout.FindControl("vMachFeedLenTextBox");
        TextBox outlen = (TextBox)FormView_FoldLayout.FindControl("vOutLenTextBox");
        TextBox diewid = (TextBox)FormView_FoldLayout.FindControl("vDieSizeWidTextBox");
        TextBox dielen = (TextBox)FormView_FoldLayout.FindControl("vDieSizeLenTextBox");
        TextBox onwid = (TextBox)FormView_FoldLayout.FindControl("vOnWidTextBox");
        TextBox onlen = (TextBox)FormView_FoldLayout.FindControl("vOnLenTextBox");
        Label ontotalup = (Label)FormView_FoldLayout.FindControl("vOnTotalUpTextBox");
        TextBox dieinches = (TextBox)FormView_FoldLayout.FindControl("vDieInchesTextBox");
        Label blankwid = (Label)FormView_FoldLayout.FindControl("vBlankWidTextBox");
        Label blanklen = (Label)FormView_FoldLayout.FindControl("vBlankLenTextBox");
        Label blanksqin = (Label)FormView_FoldLayout.FindControl("vBlankSqInchTextBox");
        TextBox leaf1 = (TextBox)FormView_FoldLayout.FindControl("vLeaf1TextBox");
        TextBox leafdesc1 = (TextBox)FormView_FoldLayout.FindControl("vLeafDesc1TextBox");
        CheckBox roll = (CheckBox)FormView_FoldLayout.FindControl("chk_roll");
        Label type = (Label)FormView_FoldLayout.FindControl("typelabel");

        if (roll.Checked)
        {
            HiddenField1.Value = "Yes";

        }
        else
        {
            HiddenField1.Value = "N";

        }

        if (frontback.Text == "")
            frontback.Text = "0";
        if (sideside.Text == "")
        {
            sideside.Text = "0";
        }
        
        if (Session["fold_layout_auto_calc"] != null ) 
        {
            try
            {
                Corrugated cor = new Corrugated();
                DataSet ds = new DataSet();
                ds = cor.CorrLayoutLen("Fold-out-len", Convert.ToString(Session["User"]), "", "", "", Convert.ToString(Session["order_folding_est"]), machine.Text, Convert.ToInt32(Session["order_folding_formno"]), Convert.ToInt32(Session["order_folding_blankno"]), board.Text.Trim(), xgrain.SelectedValue, Convert.ToDecimal(outwid.Text), Convert.ToDecimal(outlen.Text), Convert.ToDecimal(onlen.Text), Convert.ToDecimal(onwid.Text), 0, Convert.ToDecimal(gslen.Text), Convert.ToDecimal(gswid.Text), 0, Convert.ToDecimal(mflen.Text), Convert.ToDecimal(mfwid.Text), 0);

                gslen.Text = ds.Tables[0].Rows[0][2].ToString();
                gswid.Text = ds.Tables[0].Rows[0][3].ToString();
                outcut.Text = ds.Tables[0].Rows[0][10].ToString();
            }
            catch { }
        }

              


    }

    protected void xgrain_text_change(object sender, EventArgs e)
    {
        TextBox machine = (TextBox)FormView_FoldLayout.FindControl("vMachineTextBox");
        Label machinedscr = (Label)FormView_FoldLayout.FindControl("vMachDscrTextBox");
        TextBox frontback = (TextBox)FormView_FoldLayout.FindControl("vFrontBackTextBox");
        TextBox sideside = (TextBox)FormView_FoldLayout.FindControl("vSideSideTextBox");
        DropDownList xgrain = (DropDownList)FormView_FoldLayout.FindControl("vXgrainDropDown");
        TextBox board = (TextBox)FormView_FoldLayout.FindControl("vBoardTextBox");
        Label boarddesc = (Label)FormView_FoldLayout.FindControl("vBoardNameTextBox");
        Label real = (Label)FormView_FoldLayout.FindControl("vRealTextBox");
        Label caliper = (Label)FormView_FoldLayout.FindControl("vCaliperTextBox");
        TextBox costmsf = (TextBox)FormView_FoldLayout.FindControl("vCostMsfTextBox");
        TextBox costuom = (TextBox)FormView_FoldLayout.FindControl("vCostUomTextBox");
        Label weight = (Label)FormView_FoldLayout.FindControl("vWeightTextBox");
        TextBox freightcwt = (TextBox)FormView_FoldLayout.FindControl("vFreightCwtTextBox");
        TextBox freightuom = (TextBox)FormView_FoldLayout.FindControl("vFreightUomTextBox");
        DropDownList nc = (DropDownList)FormView_FoldLayout.FindControl("ddl_nc");
        Label rollwid = (Label)FormView_FoldLayout.FindControl("vRollWidTextBox");
        TextBox gswid = (TextBox)FormView_FoldLayout.FindControl("vGrosShetWidTextBox");
        TextBox gslen = (TextBox)FormView_FoldLayout.FindControl("vGrosShetLenTextBox");
        TextBox outwid = (TextBox)FormView_FoldLayout.FindControl("vOutWidTextBox");
        TextBox outcut = (TextBox)FormView_FoldLayout.FindControl("vOutCutTextBox");
        TextBox mfwid = (TextBox)FormView_FoldLayout.FindControl("vMachFeedWidTextBox");
        TextBox mflen = (TextBox)FormView_FoldLayout.FindControl("vMachFeedLenTextBox");
        TextBox outlen = (TextBox)FormView_FoldLayout.FindControl("vOutLenTextBox");
        TextBox diewid = (TextBox)FormView_FoldLayout.FindControl("vDieSizeWidTextBox");
        TextBox dielen = (TextBox)FormView_FoldLayout.FindControl("vDieSizeLenTextBox");
        TextBox onwid = (TextBox)FormView_FoldLayout.FindControl("vOnWidTextBox");
        TextBox onlen = (TextBox)FormView_FoldLayout.FindControl("vOnLenTextBox");
        Label ontotalup = (Label)FormView_FoldLayout.FindControl("vOnTotalUpTextBox");
        TextBox dieinches = (TextBox)FormView_FoldLayout.FindControl("vDieInchesTextBox");
        Label blankwid = (Label)FormView_FoldLayout.FindControl("vBlankWidTextBox");
        Label blanklen = (Label)FormView_FoldLayout.FindControl("vBlankLenTextBox");
        Label blanksqin = (Label)FormView_FoldLayout.FindControl("vBlankSqInchTextBox");

        CheckBox roll = (CheckBox)FormView_FoldLayout.FindControl("chk_roll");
        if (roll.Checked)
        {
            HiddenField1.Value = "Yes";

        }
        else
        {
            HiddenField1.Value = "N";

        }

        if (frontback.Text == "")
            frontback.Text = "0";
        if (sideside.Text == "")
        {
            sideside.Text = "0";
        }
        try
        {
            Corrugated cor = new Corrugated();
            DataSet ds = new DataSet();
            ds = cor.FoldLayMachine("FoldMachine", Convert.ToString(Session["User"]), Convert.ToString(Session["order_folding_est"]), Convert.ToInt32(Session["order_folding_formno"]), Convert.ToInt32(Session["order_folding_blankno"]), board.Text.Trim(), "", machine.Text.Trim(), xgrain.SelectedValue, HiddenField1.Value, Convert.ToDecimal(frontback.Text.Trim()), Convert.ToDecimal(sideside.Text.Trim()));

            sideside.Text = ds.Tables[0].Rows[0][0].ToString();
            frontback.Text = ds.Tables[0].Rows[0][1].ToString();
            gslen.Text = ds.Tables[0].Rows[0][2].ToString();
            gswid.Text = ds.Tables[0].Rows[0][3].ToString();
            mflen.Text = ds.Tables[0].Rows[0][4].ToString();
            mfwid.Text = ds.Tables[0].Rows[0][5].ToString();
            dielen.Text = ds.Tables[0].Rows[0][6].ToString();
            diewid.Text = ds.Tables[0].Rows[0][7].ToString();
            onlen.Text = ds.Tables[0].Rows[0][8].ToString();
            outcut.Text = ds.Tables[0].Rows[0][10].ToString();
            dieinches.Text = ds.Tables[0].Rows[0][15].ToString();
            onwid.Text = ds.Tables[0].Rows[0][16].ToString();
            onlen.Text = ds.Tables[0].Rows[0][17].ToString();
            ontotalup.Text = ds.Tables[0].Rows[0][18].ToString();
            machinedscr.Text = ds.Tables[0].Rows[0][21].ToString();
        }
        catch { }
    }

    protected void Job_Button_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Session["corr_vendor_cost_est"] = Session["order_folding_est"];
        Session["corr_vendor_cost_form"] = Session["order_folding_formno"];
        Session["corr_vendor_cost_blank"] = Session["order_folding_blankno"];

        /*Label line = (Label)FormView_FoldLayout.FindControl("vLineLabel");*/

        string vmessage = "";
        Corrugated corr = new Corrugated();
        corr.SelectPrep(UserLogin.UserName, "jobstd", "", "", Convert.ToString(Session["order_folding_est"]), Convert.ToInt32(Session["order_folding_formno"]), 0, 0, "", 0, "", 0, "", "", 0, 0, 0, 0, ref vmessage);

        if (vmessage != "")
        {

            Page.ClientScript.RegisterStartupScript(this.GetType(), "alert", "confirmAdd('" + vmessage + "');", true);

        }

        //ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "jobstd";
        //ObjectDataSource1.SelectParameters["prmLine"].DefaultValue = line.Text.Trim();

    }

}
