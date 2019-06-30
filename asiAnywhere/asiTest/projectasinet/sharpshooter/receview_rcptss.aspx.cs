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
using ASINET;
using ASIDataNS;

/// <summary>
/// Summary description for Class1
/// </summary>

namespace projectasinet.sharpshooter
{
    public partial class receview_rcptss : System.Web.UI.Page
    {
        public receview_rcptss()
        {
            //
            // TODO: Add constructor logic here
            //
        }


        protected void Page_Load(object sender, EventArgs e)
        {

            Session["my_new_rcpt"] = null;
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Session["Rowuser"] = UserLogin.UserName;
            Session["prmUser"] = UserLogin.UserName;

            if (Convert.ToString(Session["add_rcpt_list_buton"]) == "Additem")
            {
                FormView1.ChangeMode(FormViewMode.Insert);
                Session["add_rcpt_list_buton"] = "";
            }

            if (!Page.IsPostBack)
            {

            }

            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["PrmAction"].DefaultValue = "Select";
            Label heading = (Label)Master.FindControl("Label_heading");
            heading.Text = "FG Warehouse Transactions :";
            ImageButton brwsorder = (ImageButton)Master.FindControl("receview_rcpt");
            brwsorder.ImageUrl = "Images/view-receipts.jpg";

            if (Session["User"] != null)
            {

                string vUserId = UserLogin.UserName;
                string vPage = "recelist_rcpt.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;

                func1 f1 = new func1();
                //Response.Write(Page);
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");

                }

                /*if (aUsers == "external")
                {
                    customerid.Visible = false;

                }
                */

            }

        }

        protected void FormView1_DataBound(object sender, EventArgs e)
        {

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];

            if (FormView1.CurrentMode == FormViewMode.Insert)
            {
                try
                {
                    Label seq = (Label)FormView1.FindControl("vRnoTextBox");
                    TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
                    TextBox date = (TextBox)FormView1.FindControl("vDateTextBox");
                    Label create = (Label)FormView1.FindControl("vCreatedByLabel");
                    Label updateby = (Label)FormView1.FindControl("vCreate2Label");
                    Label transtime = (Label)FormView1.FindControl("vTransTimeLabel");
                    date.Text = DateTime.Now.ToShortDateString();
                    tag.Focus();
                    create.Text = UserLogin.UserName;
                    updateby.Text = UserLogin.UserName;
                    transtime.Text = DateTime.Now.ToString("HH:MM");

                    TextBox jobno2 = (TextBox)FormView1.FindControl("vJob_no2TextBox");

                    TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
                    TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
                    TextBox casunit = (TextBox)FormView1.FindControl("vCasUnitTextBox");
                    TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
                    TextBox stdcost = (TextBox)FormView1.FindControl("vStdCostTextBox");
                    TextBox t_qty = (TextBox)FormView1.FindControl("vT_QtyTextBox");
                    TextBox frtcost = (TextBox)FormView1.FindControl("vFrtCostTextBox");
                    TextBox extcost = (TextBox)FormView1.FindControl("vExtCostTextBox");
                    TextBox tot_wt = (TextBox)FormView1.FindControl("vTot_WtTextBox");
                    cases.Text = "0";
                    qtycas.Text = "0";
                    casunit.Text = "0";
                    partial.Text = "0";
                    stdcost.Text = "0";
                    t_qty.Text = "0";
                    frtcost.Text = "0";
                    extcost.Text = "0";
                    tot_wt.Text = "0";

                    itemhistory rcpt = new itemhistory();
                    DataSet dsrfqs = new DataSet();

                    dsrfqs = rcpt.ViewRcpt(UserLogin.UserName, "Addnewrcpt", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "");

                    seq.Text = dsrfqs.Tables[0].Rows[0][0].ToString();
                }
                catch { }
            }
            if (FormView1.CurrentMode == FormViewMode.Edit)
            {
                try
                {
                    TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
                    tag.Focus();
                }
                catch { }
            }
            // }
            // catch { }
        }

        protected void UpdateButton_Click(object sender, EventArgs e)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Label seq = (Label)FormView1.FindControl("vRnoLabel");
            TextBox date = (TextBox)FormView1.FindControl("vDateTextBox");
            // TextBox transtime = (TextBox)FormView1.FindControl("vTransTimeTextBox");
            TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
            TextBox pono = (TextBox)FormView1.FindControl("vPo_noTextBox");
            TextBox job = (TextBox)FormView1.FindControl("vJob_noTextBox");
            TextBox jobno2 = (TextBox)FormView1.FindControl("vJob_no2TextBox");
            TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");
            TextBox itemname = (TextBox)FormView1.FindControl("vItemNameTextBox");
            TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
            TextBox locbin = (TextBox)FormView1.FindControl("vLocBinTextBox");
            TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
            TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
            TextBox casunit = (TextBox)FormView1.FindControl("vCasUnitTextBox");
            TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
            TextBox stdcost = (TextBox)FormView1.FindControl("vStdCostTextBox");
            TextBox costuom = (TextBox)FormView1.FindControl("vCostUomTextBox");
            TextBox t_qty = (TextBox)FormView1.FindControl("vT_QtyTextBox");
            TextBox frtcost = (TextBox)FormView1.FindControl("vFrtCostTextBox");
            TextBox extcost = (TextBox)FormView1.FindControl("vExtCostTextBox");
            TextBox stackcode = (TextBox)FormView1.FindControl("vStackCodeTextBox");
            Label create = (Label)FormView1.FindControl("vCreatedByLabel");
            Label updateby = (Label)FormView1.FindControl("vCreate2Label");
            TextBox tot_wt = (TextBox)FormView1.FindControl("vTot_WtTextBox");

            try
            {
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = item.Text.Trim();
                ObjectDataSource1.SelectParameters["prmJobno"].DefaultValue = job.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPono"].DefaultValue = pono.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSeqno"].DefaultValue = seq.Text.Trim();
                ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = date.Text.Trim();
                ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = tag.Text.Trim();
                //ObjectDataSource1.SelectParameters["prmTransTime"].DefaultValue = transtime.Text.Trim();
                ObjectDataSource1.SelectParameters["prmJob_no2"].DefaultValue = jobno2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmName"].DefaultValue = itemname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmLoc"].DefaultValue = loc.Text.Trim();
                ObjectDataSource1.SelectParameters["prmLocBin"].DefaultValue = locbin.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCases"].DefaultValue = cases.Text.Trim();
                ObjectDataSource1.SelectParameters["prmQty_Cas"].DefaultValue = qtycas.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCasUnit"].DefaultValue = casunit.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPartial"].DefaultValue = partial.Text.Trim();
                ObjectDataSource1.SelectParameters["prmStdCost"].DefaultValue = stdcost.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCost_Uom"].DefaultValue = costuom.Text.Trim();
                ObjectDataSource1.SelectParameters["prmTQty"].DefaultValue = t_qty.Text.Trim();
                ObjectDataSource1.SelectParameters["prmFrtCost"].DefaultValue = frtcost.Text.Trim();
                ObjectDataSource1.SelectParameters["prmExtCost"].DefaultValue = extcost.Text.Trim();

                ObjectDataSource1.SelectParameters["prmStackCode"].DefaultValue = stackcode.Text.Trim();
                //ObjectDataSource1.SelectParameters["prmTotWt"].DefaultValue = tot_wt.Text.Trim();

                FormView1.ChangeMode(FormViewMode.ReadOnly);
            }
            catch { }



        }
        protected void InsertButton_Click(object sender, EventArgs e)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Label seq = (Label)FormView1.FindControl("vRnoTextBox");
            TextBox date = (TextBox)FormView1.FindControl("vDateTextBox");
            //TextBox transtime = (TextBox)FormView1.FindControl("vTransTimeTextBox");
            TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
            TextBox pono = (TextBox)FormView1.FindControl("vPo_noTextBox");
            TextBox job = (TextBox)FormView1.FindControl("vJob_noTextBox");
            TextBox jobno2 = (TextBox)FormView1.FindControl("vJob_no2TextBox");
            TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");
            TextBox itemname = (TextBox)FormView1.FindControl("vItemNameTextBox");
            TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
            TextBox locbin = (TextBox)FormView1.FindControl("vLocBinTextBox");
            TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
            TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
            TextBox casunit = (TextBox)FormView1.FindControl("vCasUnitTextBox");
            TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
            TextBox stdcost = (TextBox)FormView1.FindControl("vStdCostTextBox");
            TextBox costuom = (TextBox)FormView1.FindControl("vCostUomTextBox");
            TextBox t_qty = (TextBox)FormView1.FindControl("vT_QtyTextBox");
            TextBox frtcost = (TextBox)FormView1.FindControl("vFrtCostTextBox");
            TextBox extcost = (TextBox)FormView1.FindControl("vExtCostTextBox");
            TextBox stackcode = (TextBox)FormView1.FindControl("vStackCodeTextBox");
            Label create = (Label)FormView1.FindControl("vCreatedByLabel");
            Label updateby = (Label)FormView1.FindControl("vCreate2Label");
            TextBox tot_wt = (TextBox)FormView1.FindControl("vTot_WtTextBox");
            try
            {
                Session["seqno_list"] = seq.Text.Trim();

                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = item.Text.Trim();
                ObjectDataSource1.SelectParameters["prmJobno"].DefaultValue = job.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPono"].DefaultValue = pono.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSeqno"].DefaultValue = seq.Text.Trim();
                ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = date.Text.Trim();
                ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = tag.Text.Trim();
                //ObjectDataSource1.SelectParameters["prmTransTime"].DefaultValue = transtime.Text.Trim();
                ObjectDataSource1.SelectParameters["prmJob_no2"].DefaultValue = jobno2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmName"].DefaultValue = itemname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmLoc"].DefaultValue = loc.Text.Trim();
                ObjectDataSource1.SelectParameters["prmLocBin"].DefaultValue = locbin.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCases"].DefaultValue = cases.Text.Trim();
                ObjectDataSource1.SelectParameters["prmQty_Cas"].DefaultValue = qtycas.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCasUnit"].DefaultValue = casunit.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPartial"].DefaultValue = partial.Text.Trim();
                ObjectDataSource1.SelectParameters["prmStdCost"].DefaultValue = stdcost.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCost_Uom"].DefaultValue = costuom.Text.Trim();
                ObjectDataSource1.SelectParameters["prmTQty"].DefaultValue = t_qty.Text.Trim();
                ObjectDataSource1.SelectParameters["prmFrtCost"].DefaultValue = frtcost.Text.Trim();
                ObjectDataSource1.SelectParameters["prmExtCost"].DefaultValue = extcost.Text.Trim();

                ObjectDataSource1.SelectParameters["prmStackCode"].DefaultValue = stackcode.Text.Trim();
                //ObjectDataSource1.SelectParameters["prmTotWt"].DefaultValue = tot_wt.Text.Trim();

                FormView1.ChangeMode(FormViewMode.ReadOnly);
            }
            catch { }
        }

        protected void DeleteButton_Click(object sender, EventArgs e)
        {
            Label seq = (Label)FormView1.FindControl("vRnoLabel");
            Label item = (Label)FormView1.FindControl("vItemLabel");
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            try
            {
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Deletercpt";
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = seq.Text.Trim();
                ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = item.Text.Trim();

                FormView1.ChangeMode(FormViewMode.ReadOnly);
            }
            catch { }

        }
        protected void FormView1_Unload(object sender, EventArgs e)
        {

            try
            {
                Label seq = (Label)FormView1.FindControl("vRnoLabel");


                Session["seqno_list"] = seq.Text.Trim();


            }
            catch { }
        }

        protected void InserCancelButton_Click(object sender, EventArgs e)
        {
            Label seq = (Label)FormView1.FindControl("vRnoTextBox");
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];

            try
            {
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Deletercpt";
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = seq.Text.Trim();


                FormView1.ChangeMode(FormViewMode.ReadOnly);
            }
            catch { }
        }

        protected void pono_TextChange(object sender, EventArgs e)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            TextBox pono = (TextBox)FormView1.FindControl("vPo_noTextBox");
            TextBox job = (TextBox)FormView1.FindControl("vJob_noTextBox");
            TextBox jobno2 = (TextBox)FormView1.FindControl("vJob_no2TextBox");
            TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");
            TextBox itemname = (TextBox)FormView1.FindControl("vItemNameTextBox");
            TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
            TextBox locbin = (TextBox)FormView1.FindControl("vLocBinTextBox");
            TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
            TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
            TextBox casunit = (TextBox)FormView1.FindControl("vCasUnitTextBox");
            TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
            TextBox stdcost = (TextBox)FormView1.FindControl("vStdCostTextBox");
            TextBox costuom = (TextBox)FormView1.FindControl("vCostUomTextBox");
            TextBox t_qty = (TextBox)FormView1.FindControl("vT_QtyTextBox");
            TextBox frtcost = (TextBox)FormView1.FindControl("vFrtCostTextBox");
            TextBox extcost = (TextBox)FormView1.FindControl("vExtCostTextBox");
            TextBox stackcode = (TextBox)FormView1.FindControl("vStackCodeTextBox");
            Label create = (Label)FormView1.FindControl("vCreatedByLabel");
            Label updateby = (Label)FormView1.FindControl("vCreate2Label");
            TextBox tot_wt = (TextBox)FormView1.FindControl("vTot_WtTextBox");

            if (pono.Text.Trim() == "")
            {
                pono.Focus();
                return;
            }
            try
            {
                itemhistory polook = new itemhistory();
                DataSet ds = new DataSet();

                ds = polook.PoSelLookup("PoSearch", UserLogin.UserName, "PO#", "EQUAL", pono.Text.Trim());

                if (ds.Tables[0].Rows.Count == 0)
                {
                    HttpContext.Current.Response.Write("<script>alert('Invalid PO#!')</script>");
                    pono.Focus();
                }
                else
                {
                    item.Text = ds.Tables[0].Rows[0][2].ToString();
                    itemname.Text = ds.Tables[0].Rows[0][3].ToString();
                    job.Text = ds.Tables[0].Rows[0][4].ToString();
                    jobno2.Text = ds.Tables[0].Rows[0][5].ToString();
                    costuom.Text = ds.Tables[0].Rows[0][19].ToString();
                    stdcost.Text = ds.Tables[0].Rows[0][20].ToString();
                    loc.Text = ds.Tables[0].Rows[0][10].ToString();
                    locbin.Text = ds.Tables[0].Rows[0][11].ToString();
                    qtycas.Text = ds.Tables[0].Rows[0][12].ToString();
                    t_qty.Text = ds.Tables[0].Rows[0][22].ToString();
                    cases.Text = ds.Tables[0].Rows[0][24].ToString();
                    partial.Text = ds.Tables[0].Rows[0][23].ToString();
                    casunit.Text = "1";
                    if (cases.Text == "0")
                    {
                        //partial.Text = "0";
                        //t_qty.Text = "0";
                    }
                    pono.Focus();
                }
            }
            catch { }
        }

        protected void Tagtextbox_Click(object sneder, EventArgs e)
        {

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
            TextBox pono = (TextBox)FormView1.FindControl("vPo_noTextBox");
            TextBox job = (TextBox)FormView1.FindControl("vJob_noTextBox");
            TextBox jobno2 = (TextBox)FormView1.FindControl("vJob_no2TextBox");
            TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");
            TextBox itemname = (TextBox)FormView1.FindControl("vItemNameTextBox");
            TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
            TextBox locbin = (TextBox)FormView1.FindControl("vLocBinTextBox");
            TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
            TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
            TextBox casunit = (TextBox)FormView1.FindControl("vCasUnitTextBox");
            TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
            TextBox stdcost = (TextBox)FormView1.FindControl("vStdCostTextBox");
            TextBox costuom = (TextBox)FormView1.FindControl("vCostUomTextBox");
            TextBox t_qty = (TextBox)FormView1.FindControl("vT_QtyTextBox");
            TextBox frtcost = (TextBox)FormView1.FindControl("vFrtCostTextBox");
            TextBox extcost = (TextBox)FormView1.FindControl("vExtCostTextBox");
            TextBox stackcode = (TextBox)FormView1.FindControl("vStackCodeTextBox");
            Label create = (Label)FormView1.FindControl("vCreatedByLabel");
            Label updateby = (Label)FormView1.FindControl("vCreate2Label");
            TextBox tot_wt = (TextBox)FormView1.FindControl("vTot_WtTextBox");
            if (tag.Text.Trim() == "")
            {
                tag.Focus();
                return;
            }
            try
            {
                browspo fgtaglook = new browspo();
                DataSet ds = new DataSet();

                ds = fgtaglook.SelectfgtakLook("PoSearch", UserLogin.UserName, "Tag#", "EQUAL", tag.Text.Trim(), "no", "");

                if (ds.Tables[0].Rows.Count == 0)
                {
                    HttpContext.Current.Response.Write("<script>alert('Invalid Tag#!' )</script>");
                    tag.Focus();
                }
                else
                {
                    loc.Text = ds.Tables[0].Rows[0][7].ToString();
                    locbin.Text = ds.Tables[0].Rows[0][8].ToString();
                    job.Text = ds.Tables[0].Rows[0][5].ToString();
                    jobno2.Text = ds.Tables[0].Rows[0][6].ToString();
                    item.Text = ds.Tables[0].Rows[0][3].ToString();
                    itemname.Text = ds.Tables[0].Rows[0][4].ToString();
                    cases.Text = ds.Tables[0].Rows[0][18].ToString();
                    qtycas.Text = ds.Tables[0].Rows[0][9].ToString();
                    casunit.Text = ds.Tables[0].Rows[0][12].ToString();
                    partial.Text = ds.Tables[0].Rows[0][24].ToString();
                    stdcost.Text = ds.Tables[0].Rows[0][15].ToString();
                    costuom.Text = ds.Tables[0].Rows[0][13].ToString();
                    t_qty.Text = ds.Tables[0].Rows[0][16].ToString();
                    tag.Focus();
                }
            }
            catch { }
        }

        protected void ItemTextBox_Change(object sender, EventArgs e)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            TextBox pono = (TextBox)FormView1.FindControl("vPo_noTextBox");
            TextBox job = (TextBox)FormView1.FindControl("vJob_noTextBox");
            TextBox jobno2 = (TextBox)FormView1.FindControl("vJob_no2TextBox");
            TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");
            TextBox itemname = (TextBox)FormView1.FindControl("vItemNameTextBox");
            TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
            TextBox locbin = (TextBox)FormView1.FindControl("vLocBinTextBox");
            TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
            TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
            TextBox casunit = (TextBox)FormView1.FindControl("vCasUnitTextBox");
            TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
            TextBox stdcost = (TextBox)FormView1.FindControl("vStdCostTextBox");
            TextBox costuom = (TextBox)FormView1.FindControl("vCostUomTextBox");
            TextBox t_qty = (TextBox)FormView1.FindControl("vT_QtyTextBox");
            TextBox frtcost = (TextBox)FormView1.FindControl("vFrtCostTextBox");
            TextBox extcost = (TextBox)FormView1.FindControl("vExtCostTextBox");
            TextBox stackcode = (TextBox)FormView1.FindControl("vStackCodeTextBox");
            Label create = (Label)FormView1.FindControl("vCreatedByLabel");
            Label updateby = (Label)FormView1.FindControl("vCreate2Label");
            TextBox tot_wt = (TextBox)FormView1.FindControl("vTot_WtTextBox");
            if (item.Text.Trim() == "")
            {
                item.Focus();
                return;
            }
            try
            {
                itemhistory fglook = new itemhistory();
                DataSet ds = new DataSet();

                ds = fglook.PoSelLookup("ItemSearch", UserLogin.UserName, "Item", "EQUAL", item.Text.Trim());

                if (ds.Tables[0].Rows.Count == 0)
                {

                    item.Focus();
                }
                else
                {
                    itemname.Text = ds.Tables[0].Rows[0][14].ToString();
                    if (qtycas.Text == "" || qtycas.Text == "0")
                        qtycas.Text = ds.Tables[0].Rows[0][12].ToString();
                    if (stdcost.Text == "" || stdcost.Text == "0")
                        stdcost.Text = ds.Tables[0].Rows[0][20].ToString();
                    if (loc.Text == "")
                        loc.Text = ds.Tables[0].Rows[0][15].ToString();
                    if (locbin.Text == "")
                        locbin.Text = ds.Tables[0].Rows[0][16].ToString();
                    if (costuom.Text == "")
                        costuom.Text = ds.Tables[0].Rows[0][18].ToString();


                    casunit.Text = "1";
                    if (cases.Text == "0")
                    {
                        //partial.Text = "0";
                        //t_qty.Text = "0";
                    }
                    item.Focus();
                }
            }
            catch { }

        }

        protected void open_pst(object sender, EventArgs e)
        {
            Response.Redirect("finish_goods_pstingss.aspx");
        }


    }
}