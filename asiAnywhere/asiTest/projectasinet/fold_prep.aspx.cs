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
using Progress.Open4GL.Proxy;
using ASINET1;
using ASIDataNS;
using System.Text;

public partial class fold_prep : System.Web.UI.Page
{
    private string strLine = "";
        
    protected void Page_PreRender(object sender, EventArgs e)
    {
       
        try
        {
            Session["qty_type_fieldset"] = ((Label)GridView_Qty.SelectedRow.FindControl("qty_typeLabel")).Text;

            if (Convert.ToString(Session["qty_type_fieldset"]) == "4" || Convert.ToString(Session["qty_type_fieldset"]) == "2")
            {
                Qty_fieldset.Visible = false;

            }
            if (Session["fold_prep_index"] == null)
            {
                GridView_Prep.SelectedIndex = 0;
                Session["Fold_prep_line"] = ((Label)GridView_Prep.SelectedRow.FindControl("vline")).Text;

            }
        }
        catch { }
    }
    
    protected void Page_Load(object sender, EventArgs e)
    {
         
        
        FormView2.ChangeMode(FormViewMode.ReadOnly);
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        FormView_Route.ChangeMode(FormViewMode.ReadOnly);
        FormView_Qty.ChangeMode(FormViewMode.ReadOnly);
        ObjectDataSource_qtydetail.SelectParameters["prmAction"].DefaultValue = "View";
        Route_ListObjectDataSource.SelectParameters["prmActSelect"].DefaultValue = "Select";
        Detail_ObjectDataSource.SelectParameters["prmAction"].DefaultValue = "View";
        FoldPrepDataSource.SelectParameters["prmActSelect"].DefaultValue = "Select";
        GridView_Prep.DataBind();
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (!Page.IsPostBack)
        {
            
                
                        
        }
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "Fold_prep.aspx";
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
           
            GridView_Qty.SelectedIndex = Convert.ToInt32(Session["Fold_qty_index"]);
            Session["Fold_order_qty"] = GridView_Qty.SelectedRow.Cells[1].Text;
            if (Session["Fold_qty_index"] == null)
            {
                GridView_Qty.SelectedIndex = 0;
                Session["Fold_order_qty"] = GridView_Qty.SelectedRow.Cells[1].Text;
                Session["qty_type_fieldset"] = ((Label)GridView_Qty.SelectedRow.FindControl("qty_typeLabel")).Text;
            }
        }
        catch { }
             
        
        try
        {            
            Image img_mov_col = (Image)Master.FindControl("Image5");
            img_mov_col.Visible = false;

            GridView_Prep.SelectedIndex = Convert.ToInt32(Session["Fold_prep_index"]);
            if (Session["Fold_prep_index"] == null)
            {
                GridView_Prep.SelectedIndex = 0;
                Session["Fold_prep_line"] = ((Label)GridView_Prep.SelectedRow.FindControl("vline")).Text;
                
            }
        }
            catch {}
        try
        {
            GridView1.SelectedIndex = Convert.ToInt32(Session["Fold_route_index"]);
            if (Session["Fold_route_index"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["Fold_route_line"] = ((Label)GridView1.SelectedRow.FindControl("Label1")).Text;
            }


        }
        catch { }

        if (Session["Fold_prep_line"] == null || Convert.ToString(Session["Fold_prep_line"]) == "")
            {
                newadd_prep_Button.Visible = true;
            }
            if (Session["Fold_prep_line"] != null || Convert.ToString(Session["Fold_prep_line"]) != "")
            {

                newadd_prep_Button.Visible = false;
            }

            if (Session["Fold_route_line"] == null || Convert.ToString(Session["Fold_route_line"]) == "")
            {                
                rout_add_Button.Visible=true;
                rout_buildButton.Visible = true;
            }
            if (Session["Fold_route_line"] != null || Convert.ToString(Session["Fold_route_line"]) != "")
            {                
                rout_add_Button.Visible = false;
                rout_buildButton.Visible = false;
            }
            try
            {
                GridView_Qty.DataBind();
                GridView1.DataBind();
            }
            catch { }       
        

    }

    protected void GridView_Prep_SelectedIndexChanged(object sender, EventArgs e)
    {
        try
        {
            Session["Fold_prep_index"] = GridView_Prep.SelectedIndex;            
            Session["Fold_prep_line"] = ((Label)GridView_Prep.SelectedRow.FindControl("vline")).Text;

        }
        catch { }

    }
    protected void GridView_Prep_RowDataBound(object sender, GridViewRowEventArgs e)
    {

        e.Row.Attributes["onclick"] =
            ClientScript.GetPostBackClientHyperlink
                (this.GridView_Prep, "Select$" + e.Row.RowIndex);
    }

    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        
            Session["Fold_route_index"] = GridView1.SelectedIndex;            
            Session["Fold_route_line"] = ((Label)GridView1.SelectedRow.FindControl("Label1")).Text;
            
    }
    protected void GridView1_RowDataBound(object sender, GridViewRowEventArgs e)
    {
        
        e.Row.Attributes["onclick"] =
            ClientScript.GetPostBackClientHyperlink
                (this.GridView1, "Select$" + e.Row.RowIndex);
    }
    protected void update_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        TextBox sht = (TextBox)FormView2.FindControl("vSnumTextBox");
        TextBox b = (TextBox)FormView2.FindControl("vBnumTextBox");
        TextBox code = (TextBox)FormView2.FindControl("vCodeTextBox");
        TextBox qty = (TextBox)FormView2.FindControl("vQtyTextBox");
        TextBox desc = (TextBox)FormView2.FindControl("vDescTextBox");
        TextBox cost = (TextBox)FormView2.FindControl("vCostTextBox");
        DropDownList ml = (DropDownList)FormView2.FindControl("DropDownList1");
        DropDownList simon = (DropDownList)FormView2.FindControl("DropDownList2");
        TextBox mark = (TextBox)FormView2.FindControl("vMarkTextBox");
        TextBox amort = (TextBox)FormView2.FindControl("vAmortTextBox");
        Label line = (Label)FormView2.FindControl("vLineLabel");
        if (sht.Text == "")
            sht.Text = "0";
        if (b.Text == "")
            b.Text = "0";
        if (qty.Text == "")
            qty.Text = "0";
        if (cost.Text == "")
            cost.Text = "0";
        if (mark.Text == "")
            mark.Text = "0";
        if (amort.Text == "")
            amort.Text = "0";

        Corrugated corr = new Corrugated();
        bool check = corr.PrepVal(UserLogin.UserName, "PrepUpdate", "", Convert.ToString(Session["order_folding_est"]), Convert.ToInt32(Session["order_folding_formno"]), Convert.ToInt32(sht.Text), Convert.ToInt32(b.Text), code.Text, Convert.ToDecimal(qty.Text), desc.Text, Convert.ToDecimal(cost.Text), ml.SelectedValue, simon.Text, Convert.ToDecimal(mark.Text), Convert.ToDecimal(amort.Text), Convert.ToInt32(line.Text),0);
        string chec = Convert.ToString(check);

        if (chec == "True")
        {
            try
            {
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "PrepUpdate";
                ObjectDataSource1.SelectParameters["prmLine"].DefaultValue = line.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSnum"].DefaultValue = sht.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBnum"].DefaultValue = b.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCode"].DefaultValue = code.Text.Trim();
                ObjectDataSource1.SelectParameters["prmQty"].DefaultValue = qty.Text.Trim();
                ObjectDataSource1.SelectParameters["prmDesc"].DefaultValue = desc.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCost"].DefaultValue = cost.Text.Trim();
                ObjectDataSource1.SelectParameters["prmMl"].DefaultValue = ml.SelectedValue;
                ObjectDataSource1.SelectParameters["prmSimon"].DefaultValue = simon.SelectedValue;
                ObjectDataSource1.SelectParameters["prmMark"].DefaultValue = mark.Text.Trim();
                ObjectDataSource1.SelectParameters["prmAmort"].DefaultValue = amort.Text.Trim();
                Response.Write("<script>window.location.href='Fold_prep.aspx'</script>");
            }
            catch { }
        }
    }
    protected void save_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        TextBox sht = (TextBox)FormView2.FindControl("vSnumTextBox");
        TextBox b = (TextBox)FormView2.FindControl("vBnumTextBox");
        TextBox code = (TextBox)FormView2.FindControl("vCodeTextBox");
        TextBox qty = (TextBox)FormView2.FindControl("vQtyTextBox");
        TextBox desc = (TextBox)FormView2.FindControl("vDescTextBox");
        TextBox cost = (TextBox)FormView2.FindControl("vCostTextBox");
        DropDownList ml = (DropDownList)FormView2.FindControl("DropDownList1");
        DropDownList simon = (DropDownList)FormView2.FindControl("DropDownList2");
        TextBox mark = (TextBox)FormView2.FindControl("vMarkTextBox");
        TextBox amort = (TextBox)FormView2.FindControl("vAmortTextBox");
        Label line = (Label)FormView2.FindControl("vLineLabel");
        if (sht.Text == "")
            sht.Text = "0";
        if (b.Text == "")
            b.Text = "0";
        if (qty.Text == "")
            qty.Text = "0";
        if (cost.Text == "")
            cost.Text = "0";
        if (mark.Text == "")
            mark.Text = "0";
        if (amort.Text == "")
            amort.Text = "0";
        if (line.Text == "")
            line.Text = "0";

        Corrugated corr = new Corrugated();
        bool check = corr.PrepVal(UserLogin.UserName, "PrepAdd", "", Convert.ToString(Session["order_folding_est"]), Convert.ToInt32(Session["order_folding_formno"]), Convert.ToInt32(sht.Text), Convert.ToInt32(b.Text), code.Text, Convert.ToDecimal(qty.Text), desc.Text, Convert.ToDecimal(cost.Text), ml.SelectedValue, simon.SelectedValue, Convert.ToDecimal(mark.Text), Convert.ToDecimal(amort.Text), Convert.ToInt32(line.Text),0);
        string chec = Convert.ToString(check);

        if (chec == "True")
        {

            try
            {

                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "PrepAdd";
                //ObjectDataSource1.SelectParameters["prmLine"].DefaultValue = sht.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSnum"].DefaultValue = sht.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBnum"].DefaultValue = b.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCode"].DefaultValue = code.Text.Trim();
                ObjectDataSource1.SelectParameters["prmQty"].DefaultValue = qty.Text.Trim();
                ObjectDataSource1.SelectParameters["prmDesc"].DefaultValue = desc.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCost"].DefaultValue = cost.Text.Trim();
                ObjectDataSource1.SelectParameters["prmMl"].DefaultValue = ml.SelectedValue;
                ObjectDataSource1.SelectParameters["prmSimon"].DefaultValue = simon.SelectedValue;
                ObjectDataSource1.SelectParameters["prmMark"].DefaultValue = mark.Text.Trim();
                ObjectDataSource1.SelectParameters["prmAmort"].DefaultValue = amort.Text.Trim();
                Response.Write("<script>window.location.href='Fold_prep.aspx'</script>");
            }
            catch { }
        }
    }


    protected void FormView2_ItemUpdated(object sender, FormViewUpdatedEventArgs e)
    {
       
    }
    protected void ObjectDataSource1_Updated(object sender, ObjectDataSourceStatusEventArgs e)
    {
       
    }
    protected void FormView2_Unload(object sender, EventArgs e)
    {
        try
        {
            Label line = (Label)FormView2.FindControl("vLineLabel");
            Session["Fold_prep_line"] = line.Text.Trim();
        }
        catch { }
      FoldPrepDataSource.SelectParameters["prmActSelect"].DefaultValue = "Select";
    }
    protected void delete_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Label line = (Label)FormView2.FindControl("vLineLabel");
        //ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "PrepDelete";

        string estimate = Convert.ToString(Session["order_folding_est"]);
        Int32 formno = Convert.ToInt32(Session["order_folding_formno"]);
        string vmessage = "";
        Corrugated corr = new Corrugated();
        corr.SelectPrep(UserLogin.UserName, "PrepDelete", "", "", estimate, formno, 0, 0, "", 0, "", 0, "", "", 0, 0, Convert.ToInt32(line.Text),0,ref vmessage);  

        Session["Fold_prep_line"] = null;
        Session["Fold_prep_index"] = null;
        Response.Write("<script>window.location.href='Fold_prep.aspx'</script>");

    }
    protected void ObjectDataSource1_Selecting(object sender, ObjectDataSourceSelectingEventArgs e)
    {
        
       
    }
    protected void RoureSave_Click(object sender, EventArgs e)
    {
        TextBox snum = (TextBox)FormView_Route.FindControl("vSnumTextBox");
        TextBox bnum = (TextBox)FormView_Route.FindControl("vBnumTextBox");
        TextBox mcode = (TextBox)FormView_Route.FindControl("vMcodeTextBox");
        TextBox mdscr = (TextBox)FormView_Route.FindControl("vMdscrTextBox");
        TextBox pass = (TextBox)FormView_Route.FindControl("vOppassTextBox");
        TextBox nout = (TextBox)FormView_Route.FindControl("vNoutTextBox");
        TextBox mr = (TextBox)FormView_Route.FindControl("vOpmrTextBox");
        TextBox waste = (TextBox)FormView_Route.FindControl("vOpwasteTextBox");
        TextBox speed = (TextBox)FormView_Route.FindControl("vOpspeedTextBox");
        TextBox spoil = (TextBox)FormView_Route.FindControl("vOpspoilTextBox");
        TextBox crew1 = (TextBox)FormView_Route.FindControl("vOpcrewTextBox");
        TextBox crew2 = (TextBox)FormView_Route.FindControl("vOpcrew2TextBox");
        TextBox rate = (TextBox)FormView_Route.FindControl("vOpRateTextBox");
        TextBox rate2 = (TextBox)FormView_Route.FindControl("vOpRate2TextBox");
        TextBox plate = (TextBox)FormView_Route.FindControl("vPlatesTextBox");
        TextBox fount = (TextBox)FormView_Route.FindControl("vFountainsTextBox");
        if (snum.Text == "")
            snum.Text = "0";
        if (bnum.Text == "")
            bnum.Text = "0";
        if (nout.Text == "")
            nout.Text = "0";
        
        Label line = (Label)FormView_Route.FindControl("vLineTextBox");

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Corrugated corr = new Corrugated();
        bool chek = corr.FoldRouteVal(UserLogin.UserName, "RouteAdd", "", Convert.ToString(Session["order_folding_est"]), Convert.ToInt32(Session["order_folding_formno"]), Convert.ToInt32(snum.Text), Convert.ToInt32(bnum.Text), mcode.Text, mdscr.Text, Convert.ToInt32(nout.Text), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        string check = Convert.ToString(chek);

        if (check == "True")
        {       
            Detail_ObjectDataSource.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            Detail_ObjectDataSource.SelectParameters["prmAction"].DefaultValue = "RouteAdd";
            Detail_ObjectDataSource.SelectParameters["prmLine"].DefaultValue = line.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmSnum"].DefaultValue = snum.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmBnum"].DefaultValue = bnum.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmMcode"].DefaultValue = mcode.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmMdscr"].DefaultValue = mdscr.Text.Trim();

            Detail_ObjectDataSource.SelectParameters["prmNout"].DefaultValue = nout.Text.Trim();

            Response.Write("<script>window.location.href='Fold_prep.aspx'</script>");           
        }
    }

    protected void AddStdsSave_Click(object sender, EventArgs e)
    {
        TextBox snum = (TextBox)FormView_Route.FindControl("vSnumTextBox");
        TextBox bnum = (TextBox)FormView_Route.FindControl("vBnumTextBox");
        TextBox mcode = (TextBox)FormView_Route.FindControl("vMcodeTextBox");
        TextBox mdscr = (TextBox)FormView_Route.FindControl("vMdscrTextBox");
        TextBox pass = (TextBox)FormView_Route.FindControl("vOppassTextBox");
        TextBox nout = (TextBox)FormView_Route.FindControl("vNoutTextBox");
        TextBox mr = (TextBox)FormView_Route.FindControl("vOpmrTextBox");
        TextBox waste = (TextBox)FormView_Route.FindControl("vOpwasteTextBox");
        TextBox speed = (TextBox)FormView_Route.FindControl("vOpspeedTextBox");
        TextBox spoil = (TextBox)FormView_Route.FindControl("vOpspoilTextBox");
        TextBox crew1 = (TextBox)FormView_Route.FindControl("vOpcrewTextBox");
        TextBox crew2 = (TextBox)FormView_Route.FindControl("vOpcrew2TextBox");
        TextBox rate = (TextBox)FormView_Route.FindControl("vOpRateTextBox");
        TextBox rate2 = (TextBox)FormView_Route.FindControl("vOpRate2TextBox");
        TextBox plate = (TextBox)FormView_Route.FindControl("vPlatesTextBox");
        TextBox fount = (TextBox)FormView_Route.FindControl("vFountainsTextBox");
        if (snum.Text == "")
            snum.Text = "0";
        if (bnum.Text == "")
            bnum.Text = "0";
        if (nout.Text == "")
            nout.Text = "0";
        if (mr.Text == "")
            mr.Text = "0";
        if (waste.Text == "")
            waste.Text = "0";
        if (speed.Text == "")
            speed.Text = "0";
        if (spoil.Text == "")
            spoil.Text = "0";
        if (crew1.Text == "")
            crew1.Text = "0";
        if (crew2.Text == "")
            crew2.Text = "0";
        if (rate.Text == "")
            rate.Text = "0";
        if (rate2.Text == "")
            rate2.Text = "0";
        if (plate.Text == "")
            plate.Text = "0";
        if (fount.Text == "")
            fount.Text = "0";

        Label line = (Label)FormView_Route.FindControl("vLineTextBox");

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Corrugated corr = new Corrugated();
        bool chek = corr.FoldRouteVal(UserLogin.UserName, "RouteAdd", "", Convert.ToString(Session["order_folding_est"]), Convert.ToInt32(Session["order_folding_formno"]), Convert.ToInt32(snum.Text), Convert.ToInt32(bnum.Text), mcode.Text, mdscr.Text, Convert.ToInt32(nout.Text), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        string check = Convert.ToString(chek);

        if (check == "True")
        {
            Detail_ObjectDataSource.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            Detail_ObjectDataSource.SelectParameters["prmAction"].DefaultValue = "RouteAddStds";
            Detail_ObjectDataSource.SelectParameters["prmLine"].DefaultValue = line.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmSnum"].DefaultValue = snum.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmBnum"].DefaultValue = bnum.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmMcode"].DefaultValue = mcode.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmMdscr"].DefaultValue = mdscr.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmNout"].DefaultValue = nout.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpmr"].DefaultValue = mr.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpwaste"].DefaultValue = waste.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpspeed"].DefaultValue = speed.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpspoil"].DefaultValue = spoil.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpcrew"].DefaultValue = crew1.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpcrew2"].DefaultValue = crew2.Text.Trim();

            Response.Write("<script>window.location.href='Fold_prep.aspx'</script>");
        }
    }

    protected void UpdateButtonClick(object sender, EventArgs e)
    {
        Session["fold_prep_op_click"] = 1;
    }

    protected void CopyButtonClick(object sender, EventArgs e)
    {
        Session["fold_prep_op_click"] = 2;
    }
    protected void import_Click(object sender, EventArgs e)
    {
        Session["fold_prep_op_click"] = 3;
    }

    protected void BuildButtonClick(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Detail_ObjectDataSource.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        Detail_ObjectDataSource.SelectParameters["prmAction"].DefaultValue = "RouteBuild";       
        Response.Write("<script>window.location.href='Fold_prep.aspx'</script>");       
    }

    protected void RouteUpdate_Click(object sender, EventArgs e)
    {
        TextBox snum = (TextBox)FormView_Route.FindControl("vSnumTextBox");
        TextBox bnum = (TextBox)FormView_Route.FindControl("vBnumTextBox");
        TextBox mcode = (TextBox)FormView_Route.FindControl("vMcodeTextBox");
        TextBox mdscr = (TextBox)FormView_Route.FindControl("vMdscrTextBox");
        TextBox pass = (TextBox)FormView_Route.FindControl("vOppassTextBox");
        TextBox nout = (TextBox)FormView_Route.FindControl("vNoutTextBox");
        TextBox mr = (TextBox)FormView_Route.FindControl("vOpmrTextBox");
        TextBox waste = (TextBox)FormView_Route.FindControl("vOpwasteTextBox");
        TextBox speed = (TextBox)FormView_Route.FindControl("vOpspeedTextBox");
        TextBox spoil = (TextBox)FormView_Route.FindControl("vOpspoilTextBox");
        TextBox crew1 = (TextBox)FormView_Route.FindControl("vOpcrewTextBox");
        TextBox crew2 = (TextBox)FormView_Route.FindControl("vOpcrew2TextBox");
        TextBox rate = (TextBox)FormView_Route.FindControl("vOpRateTextBox");
        TextBox rate2 = (TextBox)FormView_Route.FindControl("vOpRate2TextBox");
        TextBox plate = (TextBox)FormView_Route.FindControl("vPlatesTextBox");
        TextBox fount = (TextBox)FormView_Route.FindControl("vFountainsTextBox");
        TextBox ink = (TextBox)FormView_Route.FindControl("vInksTextBox");
        TextBox coat = (TextBox)FormView_Route.FindControl("vCoatTextBox");
        
        Label line = (Label)FormView_Route.FindControl("vLineTextBox");
        if (snum.Text == "")
            snum.Text = "0";
        if (bnum.Text == "")
            bnum.Text = "0";
        if (nout.Text == "")
            nout.Text = "0";
        if (mr.Text == "")
            mr.Text = "0";
        if (waste.Text == "")
            waste.Text = "0";
        if (speed.Text == "")
            speed.Text = "0";
        if (spoil.Text == "")
            spoil.Text = "0";
        if (crew1.Text == "")
            crew1.Text = "0";
        if (crew2.Text == "")
            crew2.Text = "0";

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Corrugated corr = new Corrugated();
        bool chek = corr.FoldRouteVal(UserLogin.UserName, "RouteUpdate", "", Convert.ToString(Session["order_folding_est"]), Convert.ToInt32(Session["order_folding_formno"]), Convert.ToInt32(snum.Text), Convert.ToInt32(bnum.Text), mcode.Text, mdscr.Text, Convert.ToInt32(nout.Text),Convert.ToDecimal(mr.Text),Convert.ToInt32(waste.Text),Convert.ToInt32(speed.Text),Convert.ToDecimal(spoil.Text),Convert.ToDecimal(crew1.Text),Convert.ToDecimal(crew2.Text), 0, 0, 0, 0, 0, 0, 0, 0);
        string check = Convert.ToString(chek);

        if (check == "True")
        {

            //Detail_ObjectDataSource.SelectParameters["prmUser"].DefaultValue = snum.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmAction"].DefaultValue = "RouteUpdate";
            Detail_ObjectDataSource.SelectParameters["prmLine"].DefaultValue = line.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmSnum"].DefaultValue = snum.Text.Trim();
            //Detail_ObjectDataSource.SelectParameters["prmBnum"].DefaultValue = bnum.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmMcode"].DefaultValue = mcode.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmMdscr"].DefaultValue = mdscr.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmNout"].DefaultValue = nout.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpmr"].DefaultValue = mr.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpwaste"].DefaultValue = waste.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpspeed"].DefaultValue = speed.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpspoil"].DefaultValue = spoil.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpcrew"].DefaultValue = crew1.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpcrew2"].DefaultValue = crew2.Text.Trim();

            Session["fold_prep_op_click"] = null;

            Response.Write("<script>window.location.href='Fold_prep.aspx'</script>");
        }

    }

    protected void CopyButton_Click(object sender, EventArgs e)
    {
        TextBox snum = (TextBox)FormView_Route.FindControl("vSnumTextBox");
        TextBox bnum = (TextBox)FormView_Route.FindControl("vBnumTextBox");
        TextBox mcode = (TextBox)FormView_Route.FindControl("vMcodeTextBox");
        TextBox mdscr = (TextBox)FormView_Route.FindControl("vMdscrTextBox");
        TextBox pass = (TextBox)FormView_Route.FindControl("vOppassTextBox");
        TextBox nout = (TextBox)FormView_Route.FindControl("vNoutTextBox");
        TextBox mr = (TextBox)FormView_Route.FindControl("vOpmrTextBox");
        TextBox waste = (TextBox)FormView_Route.FindControl("vOpwasteTextBox");
        TextBox speed = (TextBox)FormView_Route.FindControl("vOpspeedTextBox");
        TextBox spoil = (TextBox)FormView_Route.FindControl("vOpspoilTextBox");
        TextBox crew1 = (TextBox)FormView_Route.FindControl("vOpcrewTextBox");
        TextBox crew2 = (TextBox)FormView_Route.FindControl("vOpcrew2TextBox");
        TextBox rate = (TextBox)FormView_Route.FindControl("vOpRateTextBox");
        TextBox rate2 = (TextBox)FormView_Route.FindControl("vOpRate2TextBox");
        TextBox plate = (TextBox)FormView_Route.FindControl("vPlatesTextBox");
        TextBox fount = (TextBox)FormView_Route.FindControl("vFountainsTextBox");

        TextBox ink = (TextBox)FormView_Route.FindControl("vInksTextBox");
        TextBox coat = (TextBox)FormView_Route.FindControl("vCoatTextBox");        

        if (snum.Text == "")
            snum.Text = "0";
        if (bnum.Text == "")
            bnum.Text = "0";
        if (nout.Text == "")
            nout.Text = "0";
        if (mr.Text == "")
            mr.Text = "0";
        if (waste.Text == "")
            waste.Text = "0";
        if (speed.Text == "")
            speed.Text = "0";
        if (spoil.Text == "")
            spoil.Text = "0";
        if (crew1.Text == "")
            crew1.Text = "0";
        if (mr.Text == "")
            mr.Text = "0";
        if (crew2.Text == "")
            crew2.Text = "0";
        if (rate.Text == "")
            rate.Text = "0";
        if (rate2.Text == "")
            rate2.Text = "0";
        if (plate.Text == "")
            plate.Text = "0";
        if (fount.Text == "")
            fount.Text = "0";
        if (ink.Text == "")
            ink.Text = "0";
        if (coat.Text == "")
            coat.Text = "0";


        Label line = (Label)FormView_Route.FindControl("vLineTextBox");

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Corrugated corr = new Corrugated();
        bool chek = corr.FoldRouteVal(UserLogin.UserName, "RouteAdd", "", Convert.ToString(Session["order_folding_est"]), Convert.ToInt32(Session["order_folding_formno"]), Convert.ToInt32(snum.Text), Convert.ToInt32(bnum.Text), mcode.Text, mdscr.Text, Convert.ToInt32(nout.Text), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        string check = Convert.ToString(chek);

        if (check == "True")
        {
            Detail_ObjectDataSource.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            Detail_ObjectDataSource.SelectParameters["prmAction"].DefaultValue = "RouteCopy";
            Detail_ObjectDataSource.SelectParameters["prmLine"].DefaultValue = line.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmSnum"].DefaultValue = snum.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmBnum"].DefaultValue = bnum.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmMcode"].DefaultValue = mcode.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmMdscr"].DefaultValue = mdscr.Text.Trim();

            Detail_ObjectDataSource.SelectParameters["prmOpmr"].DefaultValue = mr.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpwaste"].DefaultValue = waste.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpspeed"].DefaultValue = speed.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpspoil"].DefaultValue = spoil.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpcrew"].DefaultValue = crew1.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpcrew2"].DefaultValue = crew2.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpRate"].DefaultValue = rate.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpRate2"].DefaultValue = rate2.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmPlates"].DefaultValue = plate.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmFountains"].DefaultValue = fount.Text.Trim();

            Detail_ObjectDataSource.SelectParameters["prmInk"].DefaultValue = ink.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmCoat"].DefaultValue = coat.Text.Trim();                            

            Detail_ObjectDataSource.SelectParameters["prmNout"].DefaultValue = nout.Text.Trim();

            Session["fold_prep_op_click"] = null;

            Response.Write("<script>window.location.href='Fold_prep.aspx'</script>");
        }
    }

    protected void ImportButton_Click(object sender, EventArgs e)
    {
        TextBox snum = (TextBox)FormView_Route.FindControl("vSnumTextBox");
        TextBox bnum = (TextBox)FormView_Route.FindControl("vBnumTextBox");
        TextBox mcode = (TextBox)FormView_Route.FindControl("vMcodeTextBox");
        TextBox mdscr = (TextBox)FormView_Route.FindControl("vMdscrTextBox");
        TextBox pass = (TextBox)FormView_Route.FindControl("vOppassTextBox");
        TextBox nout = (TextBox)FormView_Route.FindControl("vNoutTextBox");
        TextBox mr = (TextBox)FormView_Route.FindControl("vOpmrTextBox");
        TextBox waste = (TextBox)FormView_Route.FindControl("vOpwasteTextBox");
        TextBox speed = (TextBox)FormView_Route.FindControl("vOpspeedTextBox");
        TextBox spoil = (TextBox)FormView_Route.FindControl("vOpspoilTextBox");
        TextBox crew1 = (TextBox)FormView_Route.FindControl("vOpcrewTextBox");
        TextBox crew2 = (TextBox)FormView_Route.FindControl("vOpcrew2TextBox");
        TextBox rate = (TextBox)FormView_Route.FindControl("vOpRateTextBox");
        TextBox rate2 = (TextBox)FormView_Route.FindControl("vOpRate2TextBox");
        TextBox plate = (TextBox)FormView_Route.FindControl("vPlatesTextBox");
        TextBox fount = (TextBox)FormView_Route.FindControl("vFountainsTextBox");
        TextBox ink = (TextBox)FormView_Route.FindControl("vInksTextBox");
        TextBox coat = (TextBox)FormView_Route.FindControl("vCoatTextBox");

        Label line = (Label)FormView_Route.FindControl("vLineTextBox");
        if (snum.Text == "")
            snum.Text = "0";
        if (bnum.Text == "")
            bnum.Text = "0";       
        if (nout.Text == "")
            nout.Text = "0";
        if (mr.Text == "")
            mr.Text = "0";
        if (waste.Text == "")
            waste.Text = "0";
        if (speed.Text == "")
            speed.Text = "0";
        if (spoil.Text == "")
            spoil.Text = "0";
        if (crew1.Text == "")
            crew1.Text = "0";
        if (crew2.Text == "")
            crew2.Text = "0";
        if (rate.Text == "")
            rate.Text = "0";
        if (rate2.Text == "")
            rate2.Text = "0";
        if (plate.Text == "")
            plate.Text = "0";
        if (fount.Text == "")
            fount.Text = "0";

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Corrugated corr = new Corrugated();
        bool chek = corr.FoldRouteVal(UserLogin.UserName, "RouteUpdate", "", Convert.ToString(Session["order_folding_est"]), Convert.ToInt32(Session["order_folding_formno"]), Convert.ToInt32(snum.Text), Convert.ToInt32(bnum.Text), mcode.Text, mdscr.Text, Convert.ToInt32(nout.Text), Convert.ToDecimal(mr.Text), Convert.ToInt32(waste.Text), Convert.ToInt32(speed.Text), Convert.ToDecimal(spoil.Text), Convert.ToDecimal(crew1.Text), Convert.ToDecimal(crew2.Text), 0, 0, 0, 0, 0, 0, 0, 0);
        string check = Convert.ToString(chek);

        if (check == "True")
        {

            Detail_ObjectDataSource.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            Detail_ObjectDataSource.SelectParameters["prmAction"].DefaultValue = "RouteImport";
            Detail_ObjectDataSource.SelectParameters["prmLine"].DefaultValue = line.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmSnum"].DefaultValue = snum.Text.Trim();
            //Detail_ObjectDataSource.SelectParameters["prmBnum"].DefaultValue = bnum.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmMcode"].DefaultValue = mcode.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmMdscr"].DefaultValue = mdscr.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmNout"].DefaultValue = nout.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpmr"].DefaultValue = mr.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpwaste"].DefaultValue = waste.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpspeed"].DefaultValue = speed.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpspoil"].DefaultValue = spoil.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpcrew"].DefaultValue = crew1.Text.Trim();
            Detail_ObjectDataSource.SelectParameters["prmOpcrew2"].DefaultValue = crew2.Text.Trim();

            Session["fold_prep_op_click"] = null;

            Response.Write("<script>window.location.href='Fold_prep.aspx'</script>");
        }      
    }

    protected void FormView_Route_DataBound(object sender, EventArgs e)
    {
        
        if(FormView_Route.CurrentMode == FormViewMode.Insert)
        {
           
            TextBox snum =(TextBox)FormView_Route.FindControl("vSnumTextBox");
            snum.Text = "1";
            snum.Focus();
            
            TextBox bnum = (TextBox)FormView_Route.FindControl("vBnumTextBox");
            
            TextBox mdscr = (TextBox)FormView_Route.FindControl("vMdscrTextBox");
            TextBox nout = (TextBox)FormView_Route.FindControl("vNoutTextBox");
            TextBox mr = (TextBox)FormView_Route.FindControl("vOpmrTextBox");
            TextBox waste = (TextBox)FormView_Route.FindControl("vOpwasteTextBox");
            TextBox speed = (TextBox)FormView_Route.FindControl("vOpspeedTextBox");
            TextBox spoil = (TextBox)FormView_Route.FindControl("vOpspoilTextBox");
            TextBox crew1 = (TextBox)FormView_Route.FindControl("vOpcrewTextBox");
            TextBox crew2 = (TextBox)FormView_Route.FindControl("vOpcrew2TextBox");
            TextBox rate = (TextBox)FormView_Route.FindControl("vOpRateTextBox");
            TextBox rate2 = (TextBox)FormView_Route.FindControl("vOpRate2TextBox");
            TextBox plate = (TextBox)FormView_Route.FindControl("vPlatesTextBox");
            TextBox fount = (TextBox)FormView_Route.FindControl("vFountainsTextBox");
            TextBox inks = (TextBox)FormView_Route.FindControl("vInkTextBox");
            TextBox coat = (TextBox)FormView_Route.FindControl("vCoatTextBox");

            Button addbtn = (Button)FormView_Route.FindControl("InsertButton");
            Button addstdsbtn = (Button)FormView_Route.FindControl("InsertStdsButton");

            if (HiddenField3.Value == "Add")
            {
                addstdsbtn.Visible = false;
            }
            else if (HiddenField3.Value == "AddStds")
            {
                addbtn.Visible = false;
            }

            bnum.ReadOnly = true;
            nout.ReadOnly = true;
            mdscr.ReadOnly = true;            
            rate.ReadOnly = true;
            rate2.ReadOnly = true;
            plate.ReadOnly = true;
            fount.ReadOnly = true;
            inks.ReadOnly = true;
            coat.ReadOnly = true;

            bnum.BackColor = System.Drawing.Color.Turquoise;            
            nout.BackColor = System.Drawing.Color.Turquoise;
            mdscr.BackColor = System.Drawing.Color.Turquoise;            
            rate.BackColor = System.Drawing.Color.Turquoise;
            rate2.BackColor = System.Drawing.Color.Turquoise;
            plate.BackColor = System.Drawing.Color.Turquoise;
            fount.BackColor = System.Drawing.Color.Turquoise;
            inks.BackColor = System.Drawing.Color.Turquoise;
            coat.BackColor = System.Drawing.Color.Turquoise;

            if (HiddenField3.Value == "Add")
            {
                mr.ReadOnly = true;
                waste.ReadOnly = true;
                speed.ReadOnly = true;
                spoil.ReadOnly = true;
                crew1.ReadOnly = true;
                crew2.ReadOnly = true;

                mr.BackColor = System.Drawing.Color.Turquoise;
                waste.BackColor = System.Drawing.Color.Turquoise;
                speed.BackColor = System.Drawing.Color.Turquoise;
                spoil.BackColor = System.Drawing.Color.Turquoise;
                crew1.BackColor = System.Drawing.Color.Turquoise;
                crew2.BackColor = System.Drawing.Color.Turquoise;
            }
            

        }
        if (FormView_Route.CurrentMode == FormViewMode.Edit)
        {
            TextBox bnum = (TextBox)FormView_Route.FindControl("vBnumTextBox");
            TextBox snum = (TextBox)FormView_Route.FindControl("vSnumTextBox");
            TextBox rate = (TextBox)FormView_Route.FindControl("vOpRateTextBox");
            TextBox rate2 = (TextBox)FormView_Route.FindControl("vOpRate2TextBox");
            TextBox plate = (TextBox)FormView_Route.FindControl("vPlatesTextBox");
            TextBox fount = (TextBox)FormView_Route.FindControl("vFountainsTextBox");
            TextBox nout = (TextBox)FormView_Route.FindControl("vNoutTextBox");
            TextBox opmr = (TextBox)FormView_Route.FindControl("vOpmrTextBox");  
            TextBox ink = (TextBox)FormView_Route.FindControl("vInksTextBox");
            TextBox coat = (TextBox)FormView_Route.FindControl("vCoatTextBox");
            Label type = (Label)FormView_Route.FindControl("typeLabel");
            TextBox mdscr = (TextBox)FormView_Route.FindControl("vMdscrTextBox");
            TextBox waste = (TextBox)FormView_Route.FindControl("vOpwasteTextBox");
            TextBox speed = (TextBox)FormView_Route.FindControl("vOpspeedTextBox");
            TextBox spoil = (TextBox)FormView_Route.FindControl("vOpspoilTextBox");
            TextBox crew1 = (TextBox)FormView_Route.FindControl("vOpcrewTextBox");
            TextBox crew2 = (TextBox)FormView_Route.FindControl("vOpcrew2TextBox");

            Button updbtn = (Button)FormView_Route.FindControl("UpdateButton");
            Button copybtn = (Button)FormView_Route.FindControl("copyButton");
            Button importbtn = (Button)FormView_Route.FindControl("ImportBtn");           

            if (Convert.ToInt32(Session["fold_prep_op_click"]) == 1)
            {  
                copybtn.Visible = false;
                importbtn.Visible = false;
            }
            if (Convert.ToInt32(Session["fold_prep_op_click"]) == 2)
            {
                updbtn.Visible = false;
                importbtn.Visible = false;
            }
            if (Convert.ToInt32(Session["fold_prep_op_click"]) == 3)
            {
                updbtn.Visible = false;
                copybtn.Visible = false;
            }

            snum.Focus();
            bnum.ReadOnly = true;
            rate.ReadOnly = true;
            rate2.ReadOnly = true;
            plate.ReadOnly = true;
            fount.ReadOnly = true;
            ink.ReadOnly = true;
            coat.ReadOnly = true;

            if (Convert.ToInt32(Session["fold_prep_op_click"]) == 3)
            {
                mdscr.ReadOnly = true;
                mdscr.BackColor = System.Drawing.Color.Turquoise;
                nout.ReadOnly = true;
                nout.BackColor = System.Drawing.Color.Turquoise;
                opmr.ReadOnly = true;
                opmr.BackColor = System.Drawing.Color.Turquoise;
                waste.ReadOnly = true;
                waste.BackColor = System.Drawing.Color.Turquoise;
                speed.ReadOnly = true;
                speed.BackColor = System.Drawing.Color.Turquoise;
                spoil.ReadOnly = true;
                spoil.BackColor = System.Drawing.Color.Turquoise;
                crew1.ReadOnly = true;
                crew1.BackColor = System.Drawing.Color.Turquoise;
                crew2.ReadOnly = true;
                crew2.BackColor = System.Drawing.Color.Turquoise;  
            }
            
            bnum.BackColor = System.Drawing.Color.Turquoise;
            rate.BackColor = System.Drawing.Color.Turquoise;
            rate2.BackColor = System.Drawing.Color.Turquoise;
            ink.BackColor = System.Drawing.Color.Turquoise;
            coat.BackColor = System.Drawing.Color.Turquoise;
            plate.BackColor = System.Drawing.Color.Turquoise;
            fount.BackColor = System.Drawing.Color.Turquoise;

                    }
    }
    protected void Route_Delete_Click(object sender, EventArgs e)
    {
        Label line = (Label)FormView_Route.FindControl("vLineLabel");
        Session["Fold_route_line"] = line.Text;
        Detail_ObjectDataSource.SelectParameters["prmAction"].DefaultValue = "RouteDelete";
        Detail_ObjectDataSource.SelectParameters["prmLine"].DefaultValue = Convert.ToString(Session["Fold_route_line"]);
        Session["Fold_route_line"] = null;
        Session["Fold_route_index"] = null;
        Response.Write("<script>window.location.href='Fold_prep.aspx'</script>");
    }
    protected void GridView_Qty_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["Fold_qty_index"] = GridView_Qty.SelectedIndex;
        Session["Fold_order_qty"] = GridView_Qty.SelectedRow.Cells[1].Text;       
        
    }
    protected void Add_qty_Click(object sender, EventArgs e)
    {
        TextBox qty = (TextBox)FormView_Qty.FindControl("vCorQtyTextBox");
        Session["Fold_order_qty"] = qty.Text.Trim();
        Corrugated corr = new Corrugated();
        bool check = corr.PrepVal("", "AddQty", "", Convert.ToString(Session["order_folding_est"]), Convert.ToInt32(Session["order_folding_formno"]), 0, 0, "", Convert.ToDecimal(Session["Fold_order_qty"]), "", 0, "", "", 0, 0, 0, 0);
        string chec = Convert.ToString(check);

        if (chec == "True")
        {
            ObjectDataSource_qtydetail.SelectParameters["prmAction"].DefaultValue = "QtyAdd";
            ObjectDataSource_qtydetail.SelectParameters["prmQty"].DefaultValue = Convert.ToString(Session["Fold_order_qty"]);

            Response.Write("<script>window.location.href='Fold_prep.aspx'</script>");
        }
    }
    protected void update_qty_Click(object sender, EventArgs e)
    {
        TextBox qty = (TextBox)FormView_Qty.FindControl("vCorQtyTextBox");
        Label corrid = (Label)FormView_Qty.FindControl("corridLabel");
        Session["Fold_order_qty"] = qty.Text.Trim();

        Corrugated corr = new Corrugated();
        bool check = corr.PrepVal("", "UpdateQty", "", Convert.ToString(Session["order_folding_est"]), Convert.ToInt32(Session["order_folding_formno"]), 0, 0, "", Convert.ToDecimal(Session["Fold_order_qty"]), "", 0, "", "", 0, 0, 0, Convert.ToInt32(Session["fold_est_old_qty"]));
        string chec = Convert.ToString(check);

        if (chec == "True")
        {
            ObjectDataSource_qtydetail.SelectParameters["prmAction"].DefaultValue = "QtyUpdate";
            ObjectDataSource_qtydetail.SelectParameters["prmReckey"].DefaultValue = corrid.Text.Trim();
            ObjectDataSource_qtydetail.SelectParameters["prmQty"].DefaultValue = qty.Text.Trim();

            Response.Write("<script>window.location.href='Fold_prep.aspx'</script>");
        }

    }
    protected void delete_qty_Click(object sender, EventArgs e)
    {
        Label qty = (Label)FormView_Qty.FindControl("vCorQtyLabel");
        Session["Fold_order_qty"] = qty.Text.Trim();

        Corrugated corr = new Corrugated();
        bool check = corr.PrepVal("", "DeleteQty", "", Convert.ToString(Session["order_folding_est"]), Convert.ToInt32(Session["order_folding_formno"]), 0, 0, "", Convert.ToDecimal(Session["Fold_order_qty"]), "", 0, "", "", 0, 0, 0, 0);
        string chec = Convert.ToString(check);

        if (chec == "True")
        {
            ObjectDataSource_qtydetail.SelectParameters["prmAction"].DefaultValue = "QtyDelete";
            ObjectDataSource_qtydetail.SelectParameters["prmQty"].DefaultValue = qty.Text.Trim();
            Session["Fold_qty_index"] = null;
            Response.Write("<script>window.location.href='Fold_prep.aspx'</script>");
        }

    }
    protected void new_button_prep_Click(object sender, EventArgs e)
    {
        FormView2.ChangeMode(FormViewMode.Insert);
        newadd_prep_Button.Visible = false;
    }

    
    protected void new_button_rout_Click(object sender, EventArgs e)
    {
        FormView_Route.ChangeMode(FormViewMode.Insert);
        rout_add_Button.Visible = false;
    }
    protected void FormView_Route_PreRender(object sender, EventArgs e)
    {
        
        
    }
    protected void FormView_Route_Load(object sender, EventArgs e)
    {
        
    }
    protected void FormView_Route_Unload(object sender, EventArgs e)
    {
        try
        {
            Label line = (Label)FormView_Route.FindControl("vLineLabel");
            Session["Fold_route_line"] = line.Text.Trim();
        }
        catch { }

    }


    protected void FormView_Qty_DataBound(object sender, EventArgs e)
    {
        Button update = (Button)FormView_Qty.FindControl("UpdateButton");
        Button copy = (Button)FormView_Qty.FindControl("copysaveButton");

        if (FormView_Qty.CurrentMode == FormViewMode.Insert)
        {
            TextBox qty = (TextBox)FormView_Qty.FindControl("vCorQtyTextBox");
            qty.Focus();
        }
        if (FormView_Qty.CurrentMode == FormViewMode.Edit)
        {
           
            TextBox qty = (TextBox)FormView_Qty.FindControl("vCorQtyTextBox");
            qty.Focus();

            if (HiddenField2.Value == "copy")
            {
                update.Visible = false;
            }
            else
            {
                copy.Visible = false;
            }
        }
    }

    protected void FormView_Qty_unload(object sender, EventArgs e)
    {
        try
        {
            Label qty = (Label)FormView_Qty.FindControl("vCorQtyLabel");
            Session["fold_est_old_qty"] = qty.Text;
        }
        catch { }
    }
    protected void copyButton_click(object sender, EventArgs e)
    {
        Session["fold_prep_copybutton_event"] = "copy";
        //Response.Write(copyevent);
    }
    protected void copy_save_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        TextBox sht = (TextBox)FormView2.FindControl("vSnumTextBox");
        TextBox b = (TextBox)FormView2.FindControl("vBnumTextBox");
        TextBox code = (TextBox)FormView2.FindControl("vCodeTextBox");
        TextBox qty = (TextBox)FormView2.FindControl("vQtyTextBox");
        TextBox desc = (TextBox)FormView2.FindControl("vDescTextBox");
        TextBox cost = (TextBox)FormView2.FindControl("vCostTextBox");
        DropDownList ml = (DropDownList)FormView2.FindControl("DropDownList1");
        DropDownList simon = (DropDownList)FormView2.FindControl("DropDownList2");
        TextBox mark = (TextBox)FormView2.FindControl("vMarkTextBox");
        TextBox amort = (TextBox)FormView2.FindControl("vAmortTextBox");
        Label line = (Label)FormView2.FindControl("vLineLabel");
        if (sht.Text == "")
            sht.Text = "0";
        if (b.Text == "")
            b.Text = "0";
        if (qty.Text == "")
            qty.Text = "0";
        if (cost.Text == "")
            cost.Text = "0";
        if (mark.Text == "")
            mark.Text = "0";
        if (amort.Text == "")
            amort.Text = "0";
        if (line.Text == "")
            line.Text = "0";

        Corrugated corr = new Corrugated();
        bool check = corr.PrepVal(UserLogin.UserName, "PrepAdd", "", Convert.ToString(Session["order_folding_est"]), Convert.ToInt32(Session["order_folding_formno"]), Convert.ToInt32(sht.Text), Convert.ToInt32(b.Text), code.Text, Convert.ToDecimal(qty.Text), desc.Text, Convert.ToDecimal(cost.Text), ml.SelectedValue, simon.SelectedValue, Convert.ToDecimal(mark.Text), Convert.ToDecimal(amort.Text), Convert.ToInt32(line.Text), 0);
        string chec = Convert.ToString(check);

        if (chec == "True")
        {

            try
            {

                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "PrepAdd";
                //ObjectDataSource1.SelectParameters["prmLine"].DefaultValue = sht.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSnum"].DefaultValue = sht.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBnum"].DefaultValue = b.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCode"].DefaultValue = code.Text.Trim();
                ObjectDataSource1.SelectParameters["prmQty"].DefaultValue = qty.Text.Trim();
                ObjectDataSource1.SelectParameters["prmDesc"].DefaultValue = desc.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCost"].DefaultValue = cost.Text.Trim();
                ObjectDataSource1.SelectParameters["prmMl"].DefaultValue = ml.SelectedValue;
                ObjectDataSource1.SelectParameters["prmSimon"].DefaultValue = simon.SelectedValue;
                ObjectDataSource1.SelectParameters["prmMark"].DefaultValue = mark.Text.Trim();
                ObjectDataSource1.SelectParameters["prmAmort"].DefaultValue = amort.Text.Trim();
                Response.Write("<script>window.location.href='Fold_prep.aspx'</script>");
            }
            catch { }
        }

    }

    protected void Formview2_ondatabound(object sender, EventArgs e)
    {
        if (FormView2.CurrentMode == FormViewMode.ReadOnly)
        {

            Button job = (Button)FormView2.FindControl("jobButton");
            UserClass UserLogin = (UserClass)Session["User"];

            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "Fold_prep.aspx";
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
                    try
                    {
                        job.Visible = false;
                    }
                    catch { }
                }
            }

        }

        if (FormView2.CurrentMode == FormViewMode.Edit)
        {
            Button update = (Button)FormView2.FindControl("UpdateButton");
            Button copy = (Button)FormView2.FindControl("Button1");
            
            if (Convert.ToString(Session["fold_prep_copybutton_event"]) == "copy")
            {
                update.Visible = false;
            }
            else
            {
                copy.Visible = false;
            }
        }
        if (FormView2.CurrentMode == FormViewMode.Insert)
        {
            TextBox sn = (TextBox)FormView2.FindControl("vSnumTextBox");
            sn.Text = "1";
        }
    }

    protected void reset_update_Click(object sender, EventArgs e)
    {
        FormView2.ChangeMode(FormViewMode.Edit);
    }
    protected void reset_insert_Click(object sender, EventArgs e)
    {
        FormView2.ChangeMode(FormViewMode.Insert);
    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        Session["fold_prep_copybutton_event"] = "update";
    }


    protected void copyeqtybtn_click(object sender, EventArgs e)
    {
        HiddenField2.Value = "copy";
    }
    protected void updeqtybtn_click(object sender, EventArgs e)
    {
        HiddenField2.Value = "update";
    }

    protected void reset_eqty_update_Click(object sender, EventArgs e)
    {
        FormView_Qty.ChangeMode(FormViewMode.Edit);
    }
    protected void reset_eqty_insert_Click(object sender, EventArgs e)
    {
        FormView_Qty.ChangeMode(FormViewMode.Insert);
    }

    protected void copy_eqty_save_Click(object sender, EventArgs e)
    {
        TextBox qty = (TextBox)FormView_Qty.FindControl("vCorQtyTextBox");
        Session["Fold_order_qty"] = qty.Text.Trim();
        Corrugated corr = new Corrugated();
        bool check = corr.PrepVal("", "AddQty", "", Convert.ToString(Session["order_folding_est"]), Convert.ToInt32(Session["order_folding_formno"]), 0, 0, "", Convert.ToDecimal(Session["Fold_order_qty"]), "", 0, "", "", 0, 0, 0, 0);
        string chec = Convert.ToString(check);

        if (chec == "True")
        {
            ObjectDataSource_qtydetail.SelectParameters["prmAction"].DefaultValue = "QtyAdd";
            ObjectDataSource_qtydetail.SelectParameters["prmQty"].DefaultValue = Convert.ToString(Session["Fold_order_qty"]);

            Response.Write("<script>window.location.href='Fold_prep.aspx'</script>");
        }
    }

    protected void Job_Button_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Session["corr_vendor_cost_est"] = Session["order_folding_est"];
        Session["corr_vendor_cost_form"] = Session["order_folding_formno"];
        Session["corr_vendor_cost_blank"] = Session["order_folding_blankno"];

        Label line = (Label)FormView2.FindControl("vLineLabel");

        string vmessage = "";
        Corrugated corr = new Corrugated();
        corr.SelectPrep(UserLogin.UserName, "jobstd", "", "", Convert.ToString(Session["order_folding_est"]), Convert.ToInt32(Session["order_folding_formno"]), 0, 0, "", 0, "", 0, "", "", 0, 0, Convert.ToInt32(line.Text.Trim()),0, ref vmessage);

        if (vmessage != "")
        {

            Page.ClientScript.RegisterStartupScript(this.GetType(), "alert", "confirmAdd('" + vmessage + "');", true);

        }     

        //ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "jobstd";
        //ObjectDataSource1.SelectParameters["prmLine"].DefaultValue = line.Text.Trim();
       
    }

    protected void Op_Add_Click(object sender, EventArgs e)
    {
        HiddenField3.Value = "Add";
    }
    protected void Op_AddStds_Click(object sender, EventArgs e)
    {
        HiddenField3.Value = "AddStds";
    }
}

